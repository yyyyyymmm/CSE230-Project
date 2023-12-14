module GameUtils where

import System.Directory
import System.FilePath ((</>))
import System.FilePath.Windows (FilePath)
import System.IO
import Control.Monad.IO.Class (liftIO)
import Control.Lens
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
  , _blood :: Int
  , _bonusTime :: Int
  , _combo :: Int
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

initG :: IO Game
initG = do
    musicIn <- readchooseMusic ("./assets" </> "MusicChoice.txt")
    case musicIn of 
      1 -> do
        notes <- getNotes ("./notes" </> "song.txt")
        pure $
          Game { _notes = notes
            , _end = False
            , _hit = InitState
            , _score = 0
            , _blood = 80
            , _bonusTime = 0
            }
      2 -> do
        notes <- getNotes ("./notes" </> "song.txt")
        pure $
          Game { _notes = notes
            , _end = False
            , _hit = InitState
            , _score = 0
            , _blood = 80
            , _bonusTime = 0
            }
      3 -> do
        notes <- getNotes ("./notes" </> "song.txt")
        pure $
          Game { _notes = notes
            , _end = False
            , _hit = InitState
            , _score = 0
            , _blood = 80
            , _bonusTime = 0
            }

update :: Game -> EventM () (Next Game)
update g =
  do
    let hit = if 1 `elem` concat (_notes g) then Miss else _hit g
    let newBlood = evaluateBlood (_blood g) NoneHit KeyS hit
    let newG = Game
                { _notes = move (_notes g)
                , _end = isEnd (_notes g)
                , _score = _score g
                , _hit   = hit
                , _blood = newBlood
                , _bonusTime = evaluateBonusTime ( _bonusTime g) NoneHit KeyI Miss
                } 
    continue newG

getKeyIndex :: Key -> Int
getKeyIndex k = case k of
  KeyS -> 0
  KeyJ -> 1
  KeyW -> 2
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
                    }

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

