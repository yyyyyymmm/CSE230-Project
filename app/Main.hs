module Main (main) where

import StartUI (startMenu)
import GameUI (startGame)

main :: IO ()
main = do
    mode <- startMenu
    case mode of
        1 -> startGame
        _ -> return ()
