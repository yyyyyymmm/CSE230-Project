import TestUtil
import GameUtils
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "blood decrease 1 after Miss"
    quickCheck prop_evaluateBlood
    putStrLn "bonusTime counter"
    quickCheck prop_BonusTime
    putStrLn "number of notes decreases after move function"
    quickCheck prop_notes_size
    putStrLn "all notes are positive"
    quickCheck prop_notes_positive
    putStrLn "note value minus 1 after falling"
    quickCheck prop_notes_one_unit
    putStrLn "comboCount after update"
    quickCheck prop_combo_counter_update
    putStrLn "comboCount after hit"
    quickCheck prop_combo_counter_hit
