module GameUI where

import GameUtils

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
  , padAll
  , padTop
  , padBottom
  , Padding(..)
  , fg
  )
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, unless, void, when)

data Tick = Tick

app :: App Game Tick ()
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

startGame :: IO ()
startGame = do
  chan <- newBChan 10
  void $ forkIO $ forever $ writeBChan chan Tick >> threadDelay 100000
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

appEvent :: Game -> BrickEvent () Tick -> EventM () (Next Game)
appEvent g (AppEvent Tick) = update g
appEvent g (VtyEvent (V.EvKey key [])) = keyPress key g
appEvent g _ = continue g

keyPress :: V.Key -> Game -> EventM () (Next Game)
keyPress key g = case key of
  V.KChar 's' -> continue $ hit KeyS g
  V.KChar 'S' -> continue $ hit KeyS g
  V.KChar 'j' -> continue $ hit KeyJ g
  V.KChar 'J' -> continue $ hit KeyJ g
  V.KChar 'w' -> continue $ hit KeyW g
  V.KChar 'W' -> continue $ hit KeyW g
  V.KChar 'i' -> continue $ hit KeyI g
  V.KChar 'I' -> continue $ hit KeyI g
  V.KChar 'q' -> quit g
  V.KChar 'Q' -> quit g
  _ -> continue $ g

quit :: Game -> EventM () (Next Game)
quit g = halt g

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (downAttr, V.white `on` V.rgbColor 217 209 155)
  , (upAttr, V.white `on` V.rgbColor 128 137 122)
  , (vLineAttr, V.withForeColor V.defAttr V.brightCyan )
  , (hLineAttr, V.withForeColor V.defAttr V.brightCyan)
  , (bonusTimeAttr, fg V.red `V.withStyle` V.bold)
  , (scoreAttr, V.white `on` V.rgbColor 150 80 75)
  , (quitAttr, V.white `on` V.rgbColor 217 209 155)
  , (comboAttr, V.white `on` V.rgbColor 128 137 122)
  , (eyeAttr, V.withForeColor V.defAttr V.brightCyan)
  
  ]

emptyAttr :: AttrName
emptyAttr = attrName "emptyAttr"

upAttr :: AttrName
upAttr = attrName "upAttr"

downAttr :: AttrName
downAttr = attrName "downAttr"

vLineAttr :: AttrName
vLineAttr = attrName "vLineAttr"

hLineAttr :: AttrName
hLineAttr = attrName "hLineAttr"

bonusTimeAttr :: AttrName
bonusTimeAttr = attrName "bonusTimeAttr"

scoreAttr :: AttrName
scoreAttr = attrName "score"

quitAttr :: AttrName
quitAttr = attrName "quit"

comboAttr :: AttrName
comboAttr = attrName "combo"

eyeAttr :: AttrName
eyeAttr = attrName "eye"
drawUI :: Game -> [Widget ()]
drawUI g =
  if (_end g) then 
    [C.vCenter $ C.hCenter $ drawGameOver g] 
  else (
    [ C.vCenter $ hBox $ 
      [vBox $ [ drawScore (_score g, _combo g, _blood g), padAll 1 (str " "), drawHit (_hit g) ]
        , drawBlock g
        , drawNotes g
        , vBox $ [drawGuide, padTop (Pad 1) $ drawBonusTime (_bonusTime g)]
      ]
    ]
  )

drawNotes :: Game -> Widget ()
drawNotes g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Rhythm Dash ")
  $ vBox $ map hBox grid
  where
    grid = [[drawGridAt x y | x <- [0..17]] | y <- [19, 18..0]]
    drawGridAt x y
      | y >= 6 && y < 9 && x `elem` (_notes g) !! 0 = withAttr downAttr (str "     ")
      | y >= 11 && y < 14 && x `elem` (_notes g) !! 1 = withAttr upAttr (str "     ")
      | x == 0 && ((y >= 6 && y < 9) || (y >= 11 && y < 14)) = withAttr vLineAttr (str "|")
      | x == 2 && ((y >= 6 && y < 9) || (y >= 11 && y < 14)) = withAttr vLineAttr (str "|")
      | otherwise = withAttr emptyAttr (str "     ")

drawGameOver :: Game -> Widget ()
drawGameOver g = withBorderStyle BS.unicodeBold
  $ hLimit 100
  $ B.borderWithLabel (str " Game over ")
  $ vBox $ [withAttr scoreAttr $ C.hCenter $ str ("     Final score: "++ (show $ _score g))
  , withAttr comboAttr $ C.hCenter $ str ("     Maximum combo: "++ (show $ _comboMax g))
  , withAttr quitAttr $ C.hCenter $ str ("     Press Q to go back to main page or press R to restrart.     ")
  ]

drawGuide :: Widget ()
drawGuide = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Guidance ")
  $ hLimit 25
  $ vLimit 30
  $ padAll 1
  $ vBox $ [C.hCenter $ str $ "Hit Upper Line   J", padTop (Pad 1) (str " ")
          , C.hCenter $ str $ "Hit Lower Line   S", padTop (Pad 1) (str " ")
          , C.hCenter $ str $ "Quit             Q"]


drawScore :: (Int, Int, Int) -> Widget ()
drawScore (score, combo, blood) = withBorderStyle BS.unicodeBold 
  $ B.borderWithLabel (str " ScoreBoard ")
  $ hLimit 15
  $ vLimit 30
  $ padAll 1
  $ vBox $ [C.hCenter $ str $ ("Score : " ++ show score), padTop (Pad 1) (str " ")
          , C.hCenter $ str $ ("Combo : " ++ show combo), padTop (Pad 1) (str " ")
          , C.hCenter $ str $ ("Blood : " ++ show blood)]
  -- $ vBox $ [C.hCenter $ str $ show s]

drawHit :: HitState -> Widget ()
drawHit s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Hit ")
  $ hLimit 15
  $ vLimit 15
  $ padAll 1
  $ vBox $ [C.hCenter $ str $ show $ s]

drawBonusTime :: Int -> Widget ()
drawBonusTime t = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Bonus Time ")
  $ hLimit 25
  $ vLimit 15
  $ C.hCenter
  $ padAll 1
  $ if t > 0 then
      withAttr bonusTimeAttr $ str $ show $ t
  else
      str $ show $ t

drawBlock :: Game -> Widget ()
drawBlock g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Boxman ")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [19, 18..0]]
    cellsInRow y = [(drawCoord x y) | x <- [0..2]]
    drawCoord x y 
        | x == 0 && (y >= 1 && y < 4) && (_lineNumber g) == 0 = withAttr vLineAttr (str "|")
        | x == 2 && (y >= 1 && y < 4) && (_lineNumber g) == 0 = withAttr vLineAttr (str "|")
        | x == 0 && (y >= 6 && y < 9) && (_lineNumber g) == 1 = withAttr vLineAttr (str "|")
        | x == 2 && (y >= 6 && y < 9) && (_lineNumber g) == 1 = withAttr vLineAttr (str "|")
        | x == 0 && (y >= 11 && y < 14) && (_lineNumber g) == 2 = withAttr vLineAttr (str "|")
        | x == 2 && (y >= 11 && y < 14) && (_lineNumber g) == 2 = withAttr vLineAttr (str "|")
        | x == 0 && (y >= 16 && y < 19) && (_lineNumber g) == 3 = withAttr vLineAttr (str "|")
        | x == 2 && (y >= 16 && y < 19) && (_lineNumber g) == 3 = withAttr vLineAttr (str "|")
        | x == 1 && y == 1 && (_lineNumber g) == 0 = withAttr hLineAttr (str "_____")
        | (x == 0 || x == 1) && y == 4 && (_lineNumber g) == 0 = withAttr hLineAttr (str "_____")
        | x == 1 && y == 6 && (_lineNumber g) == 1 = withAttr hLineAttr (str "_____")
        | (x == 0 || x == 1) && y == 9 && (_lineNumber g) == 1 = withAttr hLineAttr (str "_____")
        | x == 1 && y == 11 && (_lineNumber g) == 2 = withAttr hLineAttr (str "_____")
        | (x == 0 || x == 1) && y == 14 && (_lineNumber g) == 2 = withAttr hLineAttr (str "_____")
        | x == 1 && y == 16 && (_lineNumber g) == 3 = withAttr hLineAttr (str "_____")
        | (x == 0 || x == 1) && y == 19 && (_lineNumber g) == 3 = withAttr hLineAttr (str "_____")
        | x == 1 && y == 2 && (_lineNumber g) == 0 = withAttr eyeAttr (str " OvO ")
        | x == 1 && y == 7 && (_lineNumber g) == 1 = withAttr eyeAttr (str " OvO ")
        | x == 1 && y == 12 && (_lineNumber g) == 2 = withAttr eyeAttr (str " OvO ")
        | x == 1 && y == 17 && (_lineNumber g) == 3 = withAttr eyeAttr (str " OvO ")
        | otherwise             = withAttr emptyAttr (str "     ")


