module GameUtils where

import System.Directory
import System.FilePath ((</>))
import System.FilePath.Windows (FilePath)
import System.IO
import Control.Monad.IO.Class (liftIO)
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
  { _notes       :: [[Int]]
  , _end       :: Bool
  } 

getNotes :: FilePath -> IO [[Int]]
getNotes path = do
  notes <- readFile path 
  return $ read notes

isEnd :: [[Int]] -> Bool
isEnd [[], []] = True
isEnd _ = False

move :: [[Int]] -> [[Int]]
move = map (filter (>0) . map (\x -> x - 1))

initG :: IO Game
initG = do
    notes <- getNotes ("./notes" </> "song.txt")
    pure $
      Game { _notes = notes
        , _end = False
        }

update :: Game -> EventM () (Next Game)
update g =
  if (_end g) then do
    continue g
  else do
    let nextG = Game
                { _notes = move (_notes g)
                , _end = isEnd (_notes g)
                } 
    continue nextG