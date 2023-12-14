module Main (main) where

import StartUI (startMenu)
import GameUI (startGame)
import MusicUI (chooseMusic)

main :: IO ()
main = do
  mode <- startMenu
  case mode of
    1 -> startGame
    2 -> do
      _ <- chooseMusic
      main
    _ -> return ()