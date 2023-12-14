module Main (main) where

import StartUI (startMenu)
import GameUI (startGame)
import MusicUI (chooseMusic)
import Control.Monad (replicateM_)

main :: IO ()
main = replicateM_ 10 (do
  mode <- startMenu
  case mode of
    1 -> startGame
    2 -> do
      _ <- chooseMusic
      main
    _ -> return ())