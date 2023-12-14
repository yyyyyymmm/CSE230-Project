module MusicUI where

-- import ChooseMusicMode

import System.Exit (exitSuccess)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName, simpleMain
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, padBottom, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
--import Data.Sequence (Seq)
--import qualified Data.Sequence as S

titleAttr :: AttrName
titleAttr = attrName "title"
modeAttr :: AttrName
modeAttr = attrName "mode"
musicAttr :: AttrName
musicAttr = attrName "music"
quitAttr :: AttrName
quitAttr = attrName "quit"

myBorderStyle :: BS.BorderStyle
myBorderStyle = BS.BorderStyle { BS.bsCornerTL = '╔'
                                , BS.bsCornerTR = '╗'
                                , BS.bsCornerBL = '╚'
                                , BS.bsCornerBR = '╝'
                                , BS.bsIntersectFull = '╬'
                                , BS.bsIntersectL = '╠'
                                , BS.bsIntersectR = '╣'
                                , BS.bsIntersectT = '╦'
                                , BS.bsIntersectB = '╩'
                                , BS.bsHorizontal = '═'
                                , BS.bsVertical = '║'
                                }

ui :: Widget ()
ui =
  C.center
    $ vBox [ C.hCenter $ withAttr titleAttr $ str "!! Rhythm Dash !!"
           , C.hCenter $ str "^o^"
           , C.hCenter $ withBorderStyle myBorderStyle $ B.borderWithLabel (str " Choose Music ") $ hLimit 80
          --  , padTop (Pad 2) $ vBox
           $ vBox
           [ withAttr modeAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Music 1"
                  , C.hCenter $ str "Press <1>"
                  ]
           , withAttr musicAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Music 2"
                  , C.hCenter $ str "Press <2>"
                  ]
           , withAttr modeAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Music 3"
                  , C.hCenter $ str "Press <3>"
                  ]
           , withAttr quitAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Quit"
                  , C.hCenter $ str "Press <q>"
                  ]
           ]
    ]

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, V.white `on` V.blue)
    , (modeAttr, V.white `on` V.rgbColor 150 80 75)
    , (musicAttr, V.white `on` V.rgbColor 128 137 122)
    , (quitAttr, V.white `on` V.rgbColor 217 209 155)
    ]

app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]  -- draw what "ui" defines
  , appHandleEvent  = handleEvent -- handle keyboard events
  , appStartEvent   = return      -- nothing
  -- , appAttrMap      = const $ attrMap V.defAttr [] -- default attributes
  , appAttrMap      = const theMap
  , appChooseCursor = neverShowCursor -- never show cursor
  }

handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent n (VtyEvent (V.EvKey V.KEsc        _)) = halt $ Just 0
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt $ Just 0
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt $ Just 0
handleEvent n (VtyEvent (V.EvKey (V.KChar '1') [])) = do
    liftIO $ writeMusicChoice 1
    halt $ Just 1
handleEvent n (VtyEvent (V.EvKey (V.KChar '2') [])) = do
    liftIO $ writeMusicChoice 2
    halt $ Just 2
handleEvent n (VtyEvent (V.EvKey (V.KChar '3') [])) = do
    liftIO $ writeMusicChoice 3
    halt $ Just 3
handleEvent n _ = continue n

writeMusicChoice :: Int -> IO ()
writeMusicChoice n = do
  writeFile "./assets/MusicChoice.txt" (show n)

chooseMusic :: IO Int
chooseMusic = do
  mode <- defaultMain app Nothing
  case mode of
    Nothing -> exitSuccess
    Just n -> return n
