module Lab6 where

import           Data.List
import           Lecture6
import           System.Random
import           Test.QuickCheck


-- Exercises

-- 1)

exM' :: Integer -> Integer -> Integer -> Integer
exM' b 1 m = 1
exM' b x m = ((b^2) `mod` m) * (exM' b (div x 2) m)

prop_ExpEquiv :: (NonNegative Integer) -> (NonNegative Integer) -> (Positive Integer) -> Bool
prop_ExpEquiv (NonNegative x) (NonNegative y) (Positive z) = expM x y z == exM' x y z

check_ExpEquiv = quickCheck prop_ExpEquiv


-- 2)




-- 3)

composites :: [Integer]
composites = [x | x <- [2..], not . prime $ x]
