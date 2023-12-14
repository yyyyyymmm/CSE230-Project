module StartUI where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import qualified Graphics.Vty as V

import System.Exit (exitSuccess)

startMenu :: IO Int
-- startMenu = defaultMain app Nothing >>= maybe exitSuccess return
startMenu = do
    mode <- defaultMain app Nothing
    case mode of
        Just 1 -> return 1
        _ -> exitSuccess

app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui =
  C.center
    $ vBox [ C.hCenter $ withAttr titleAttr $ str "!! Rhythm Dash !!"
           , C.hCenter $ str "^o^"
           , C.hCenter $ withBorderStyle myBorderStyle $ B.borderWithLabel (str " Menu ") $ hLimit 80
           $ vBox
           [ withAttr modeAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Start Game"
                  , C.hCenter $ str "Press <1>"
                  ]
           , withAttr quitAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Quit"
                  , C.hCenter $ str "Press <q>"
                  ]
           ]
    ]

handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent _ (VtyEvent (V.EvKey V.KEsc        _)) = halt Nothing
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt Nothing
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt Nothing
handleEvent _ (VtyEvent (V.EvKey (V.KChar '1') [])) = halt $ Just 1
handleEvent _ _ = continue Nothing

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, V.white `on` V.blue)
    , (modeAttr, V.white `on` V.rgbColor 150 80 75)
    , (quitAttr, V.white `on` V.rgbColor 217 209 155)
    ]

titleAttr :: AttrName
titleAttr = attrName "title"
modeAttr :: AttrName
modeAttr = attrName "mode"
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
