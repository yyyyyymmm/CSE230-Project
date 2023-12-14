module StartUI where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import qualified Graphics.Vty as V
import System.FilePath ((</>))
import System.Exit (exitSuccess)

startMenu :: IO Int
-- startMenu = defaultMain app Nothing >>= maybe exitSuccess return
startMenu = do
    musicChoice <- readchooseMusic ("./assets" </> "MusicChoice.txt")
    mode <- defaultMain (app musicChoice) Nothing
    case mode of
        Just 1 -> return 1
        Just 2 -> return 2
        _ -> exitSuccess
readchooseMusic :: FilePath -> IO Int
readchooseMusic path = do
  musicInt <- readFile path 
  return $ read musicInt

app :: Int -> App (Maybe Int) e ()
app musicChoice = App
  { appDraw         = const [ui musicChoice]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  , appChooseCursor = neverShowCursor
  }

ui :: Int -> Widget ()
ui musicChoice =
  C.center
    $ vBox [ C.hCenter $ withAttr titleAttr $ str "!! Rhythm Dash !!"
           , C.hCenter $ str "^o^"
           , C.hCenter $ withBorderStyle myBorderStyle $ B.borderWithLabel (str " Menu ") $ hLimit 80
          --  , B.borderWithLabel (str " Menu ")
          --  , padTop (Pad 2) $ vBox
           $ vBox
           [ withAttr modeAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Game Mode"
                  , C.hCenter $ str "Press <1>"
                  ]
           , withAttr musicAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Choose Music"
                  , C.hCenter $ str "Press <2>"
                  ]
           , withAttr quitAttr $ hBox [ C.hCenter $ padRight (Pad 4) $ str "Quit"
                  , C.hCenter $ str "Press <q>"
                  ]
           ]
           , C.hCenter $ str ("Current Music: " ++ show musicChoice)
    ]

handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent _ (VtyEvent (V.EvKey V.KEsc        _)) = halt Nothing
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt Nothing
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt Nothing
handleEvent _ (VtyEvent (V.EvKey (V.KChar '1') [])) = halt $ Just 1
handleEvent n (VtyEvent (V.EvKey (V.KChar '2') [])) = halt $ Just 2
handleEvent _ _ = continue Nothing

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (titleAttr, V.white `on` V.blue)
    , (modeAttr, V.white `on` V.rgbColor 150 80 75)
    , (quitAttr, V.white `on` V.rgbColor 217 209 155)
    , (musicAttr, V.white `on` V.rgbColor 128 137 122)
    ]

titleAttr :: AttrName
titleAttr = attrName "title"
modeAttr :: AttrName
modeAttr = attrName "mode"
quitAttr :: AttrName
quitAttr = attrName "quit"
musicAttr :: AttrName
musicAttr = attrName "music"
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
