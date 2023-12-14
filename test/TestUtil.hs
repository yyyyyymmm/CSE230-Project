module TestUtil where
import Test.QuickCheck
import Control.Monad (liftM2)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import GameUtils
import Brick (runEventM)
instance Arbitrary HitState where
  arbitrary = do
    x <- elements [Perfect, Good, Miss]
    return x

instance Arbitrary Process where
  arbitrary = do
    x <- elements [NoneHit , Hit ]
    return x

instance Arbitrary Key where
  arbitrary = do
    x <- elements [KeyW , KeyS , KeyJ , KeyI]
    return x

newtype BonusTime = BonusTime Int deriving Show

instance Arbitrary BonusTime where
    arbitrary = BonusTime <$> choose (0, 30)


prop_evaluateBlood:: Int -> Process -> Key -> HitState -> Bool
prop_evaluateBlood blood e k s 
    | newblood == blood - 1 = s == Miss && e == NoneHit
    | newblood == blood + 2 = s == Perfect && e == Hit && (k == KeyW || k == KeyI)
    | otherwise = newblood == blood
    where 
        newblood = evaluateBlood blood e k s


prop_BonusTime :: BonusTime -> Process -> Key -> HitState -> Bool
prop_BonusTime (BonusTime t) e k s
  | newT == 30 = e == Hit && (k == KeyI || k == KeyW) && s == Good || ( t == 30 && e == Hit)
  | newT == t - 1 = t > 0 
  | otherwise = newT == t && e == Hit || t == 0 
  where
      newT = evaluateBonusTime t e k s

-- move
prop_notes_size :: [[Int]] -> Property
prop_notes_size s = length s == 4 ==> all (\(a, b) -> length a <= length b) (zip newS s)
  where newS = move s

prop_notes_positive :: [[Int]] -> Bool
prop_notes_positive = all (>0) . concat . move

prop_notes_one_unit :: [[Int]] -> Bool
prop_notes_one_unit s = all (`elem` (concat s)) (map (\h -> h+1) (concat (move s)))


-- evaluateCombo
-- 反向核对 NoneHit 不改变compo计数 
prop_combo_counter_update :: Int -> HitState -> Bool
prop_combo_counter_update c hs = case evaluateCombo c NoneHit hs of
    0       -> hs == Miss || c == 0
    newC    -> newC == c

-- Hit counter ++
prop_combo_counter_hit :: Int -> HitState -> Bool
prop_combo_counter_hit c hs = case evaluateCombo c Hit hs of
    0       -> hs == Miss || (hs /= Miss && c == -1)
    newC    -> newC == c+1

