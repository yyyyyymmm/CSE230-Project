module GameUtils where

import System.Directory
import System.FilePath ((</>))
import System.FilePath.Windows (FilePath)
import System.IO
import System.Process
import System.Posix.Signals
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Lens
import Control.Concurrent
import Brick
  ( App(..)
  , BrickEvent(..)
  , EventM
  , Next
  , AttrMap
  , AttrName
  , Widget
  , continue
  , halt
  , str
  , withBorderStyle
  , attrMap
  , attrName
  , withAttr
  , emptyWidget
  , hLimit
  , vLimit
  , vBox
  , hBox
  , on
  , customMain
  , neverShowCursor
  )

data Game = Game
  { _notes :: [[Int]]
  , _end :: Bool
  , _hit :: HitState
  , _score :: Int
  , _music :: ProcessHandle
  , _blood :: Int
  , _bonusTime :: Int
  , _combo :: Int
  , _comboMax :: Int
  , _lineNumber :: Int
  } 

data Process = Hit | NoneHit
    deriving (Show, Eq, Ord)

data HitState = Perfect | Good | Miss | InitState
  deriving (Show, Eq, Ord)

data Key = KeyS | KeyJ | KeyW | KeyI
    deriving (Show, Eq, Ord)

getNotes :: FilePath -> IO [[Int]]
getNotes path = do
  notes <- readFile path 
  return $ read notes

readchooseMusic :: FilePath -> IO Int
readchooseMusic path = do
  musicInt <- readFile path 
  return $ read musicInt


isEnd :: [[Int]] -> Bool
isEnd [[], []] = True
isEnd _ = False

move :: [[Int]] -> [[Int]]
move = map (filter (>0) . map (\x -> x - 1))

initGame :: IO Game
initGame = do
    musicIn <- readchooseMusic ("./assets" </> "MusicChoice.txt")
    case musicIn of 
      1 -> do
        notes <- getNotes ("./notes" </> "song.txt")
        music <- playMusic("./music" </> "song.mp3")
        pure $
          Game { _notes = notes
            , _end = False
            , _hit = InitState
            , _score = 0
            , _blood = 80
            , _bonusTime = 0
            , _combo = 0
            , _comboMax = 0
            , _lineNumber = -1
            , _music = music
            }
      2 -> do
        notes <- getNotes ("./notes" </> "song.txt")
        music <- playMusic("./music" </> "song.mp3")
        pure $
          Game { _notes = notes
            , _end = False
            , _hit = InitState
            , _score = 0
            , _blood = 80
            , _bonusTime = 0
            , _combo = 0
            , _comboMax = 0
            , _lineNumber = -1
            , _music = music
            }
      3 -> do
        notes <- getNotes ("./notes" </> "song.txt")
        music <- playMusic ("./music" </> "song.mp3")
        pure $
          Game { _notes = notes
            , _end = False
            , _hit = InitState
            , _score = 0
            , _blood = 80
            , _bonusTime = 0
            , _combo = 0
            , _comboMax = 0
            , _lineNumber = -1
            , _music = music
        }

-- playMusic :: FilePath -> IO ProcessHandle
-- playMusic filePath = do
--     let command = "afplay"
--         args = [filePath]
--     (_, _, _, processHandle) <- createProcess (proc command args)
--     return processHandle

playMusic :: FilePath -> IO ProcessHandle
playMusic filePath = do
    let command = "mpv"
        args = [filePath, "--no-terminal"]
    (_, _, _, processHandle) <- createProcess (proc command args)
    return processHandle

stopMusic :: ProcessHandle -> IO ()
stopMusic processHandle = do
    terminateProcess processHandle
    _ <- waitForProcess processHandle
    return ()

update :: Game -> EventM () (Next Game)
update g =
  if (_end g) || ((_blood g) <= 0) then do
    liftIO $ stopMusic (_music g)
    liftIO $ writeBestResult (_score g, _combo g)

    continue g
  else do
    let hit = if 1 `elem` concat (_notes g) then Miss else _hit g
    let newBlood = evaluateBlood (_blood g) NoneHit KeyS hit
    let newCombo = evaluateCombo (_combo g) NoneHit hit
    let newG = Game
                { _notes = move $ _notes g
                , _end = isEnd $ _notes g
                , _score = _score g
                , _music = _music g
                , _hit   = hit
                , _blood = newBlood
                , _bonusTime = evaluateBonusTime ( _bonusTime g) NoneHit KeyI Miss
                , _combo = newCombo
                , _comboMax = _comboMax g
                , _lineNumber = _lineNumber g
                } 
    continue newG


getKeyIndex :: Key -> Int
getKeyIndex k = case k of
  KeyS -> 1
  KeyJ -> 2
  KeyW -> 0
  KeyI -> 3

getHitState :: Int -> HitState
getHitState x
  | x == 1 = Perfect
  | x > 1 && x <= 4 = Good
  | otherwise = Miss

hit :: Key -> Game -> Game
hit k g =
  let i = getKeyIndex k
      n = _notes g
  in if null (n !! i)
       then g
       else let s = getHitState (head (n !! i))
                v = case s of
                  Perfect -> 2
                  Good -> 1
                  Miss -> 0
                newBlood = evaluateBlood (_blood g) Hit KeyS s
                newCombo = evaluateCombo (_combo g) Hit s
                times = if _bonusTime g > 0 then 2 else 1
            in Game { _notes = if s == Miss then n else n & element i %~ tail
                    , _end = _end g
                    , _hit = s
                    , _score = _score g + v
                    , _blood = newBlood
                    , _bonusTime = evaluateBonusTime ( _bonusTime g) Hit KeyW s
                    , _combo = newCombo
                    , _comboMax = max newCombo (_comboMax g)
                    , _lineNumber = i
                    , _music = _music g
                    }

-- hitTool function
hitTool :: Key -> Game -> Game
hitTool k g = 
  let i = getKeyIndex k
      n = _notes g
  in if null (n !! i)
       then g
      else let s = getHitState (head (n !! i))
               v = case s of
                  Perfect -> 2
                  Good -> 1
                  Miss -> 0
               newBlood = evaluateBlood (_blood g) Hit KeyW s
               newCombo = evaluateCombo (_combo g) Hit s
            in Game { _notes       = if s == Miss then n else move (n & element i .~ tail (n!!i))
                    , _hit   = s
                    , _end       = _end g
                    , _score      = _score g 
                    , _blood = newBlood
                    , _bonusTime = evaluateBonusTime (_bonusTime g) Hit KeyI s
                    , _combo = newCombo
                    , _comboMax   = max newCombo (_comboMax g)
                    , _lineNumber = i
                    , _music = _music g
                    }

-- Util Function

evaluateBlood :: Int -> Process -> Key -> HitState -> Int
evaluateBlood blood Hit key state
    | (key == KeyW || key == KeyI) && state == Perfect = blood + 2
evaluateBlood blood NoneHit _ Miss  = blood - 1
evaluateBlood blood _ _ _ = blood

evaluateBonusTime:: Int -> Process -> Key -> HitState -> Int
evaluateBonusTime t Hit KeyI Good = 30
evaluateBonusTime t Hit KeyW Good = 30
evaluateBonusTime t Hit _ _ = t
evaluateBonusTime t _ _ _ 
  | t > 0  = t - 1 
  | otherwise = 0

evaluateHit :: Int -> (HitState, Int)
evaluateHit h
  | h == 1      = (Perfect, 5)
  | h > 5       = (Miss, 0)
  | otherwise   = (Good, 3)

evaluateCombo :: Int -> Process -> HitState -> Int
evaluateCombo _ _ Miss = 0
evaluateCombo combo NoneHit _    = combo
evaluateCombo combo Hit  _    = combo+1

writeBestResult :: (Int,Int) -> IO ()
writeBestResult (score, combo) = do
    currentResult <- readBestResult
    musicChoice <- readchooseMusic ("./assets" </> "MusicChoice.txt")
    let index = musicChoice - 1

    if index < 0 || index >= length currentResult
    then return ()
    else do
        let currentScoreCombo = currentResult !! index
        if currentScoreCombo == [] then do
            let results = replaceAtIndex index [score, combo] currentResult
            writeFile "./assets/bestResult.txt" (show results)
        else
            if score > head currentScoreCombo then do
                let results = replaceAtIndex index [score, combo] currentResult
                writeFile "./assets/bestResult.txt" (show results)
            else if score == head currentScoreCombo && combo > (currentScoreCombo !! 1) then do
                let results = replaceAtIndex index [score, combo] currentResult
                writeFile "./assets/bestResult.txt" (show results)
            else return ()

replaceAtIndex :: Int -> [Int] -> [[Int]] -> [[Int]]
replaceAtIndex n newList list
  | n < 0 || n >= length list = list  -- If index is out of bounds, return the original list
  | otherwise = take n list ++ [newList] ++ drop (n + 1) list

readBestResult :: IO [[Int]]
readBestResult = do
  resultStr <- readFile "./assets/bestResult.txt"
  return $ read resultStr

getBestResult :: Int -> IO [Int]
getBestResult musicChoice = do
    bestResult <- readBestResult
    return $ bestResult !! (musicChoice - 1)