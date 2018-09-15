module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1) 
    return (p:ps)

-----------
-- LAB 2 --
-----------

-- Excercise 1 --

countProbs :: [Float] -> [Int]
countProbs [x]
    | x < 0.25 = [1,0,0,0]
    | x < 0.5 = [0,1,0,0]
    | x < 0.75 = [0,0,1,0]
    | otherwise = [0,0,0,1]
countProbs (x:xs)
    | x < 0.25 = zipWith (+) [1,0,0,0] (countProbs xs)
    | x < 0.5 = zipWith (+) [0,1,0,0] (countProbs xs)
    | x < 0.75 = zipWith (+) [0,0,1,0] (countProbs xs)
    | otherwise = zipWith (+) [0,0,0,1] (countProbs xs)

testProbs :: Int -> IO [Int]
testProbs n = fmap countProbs (probs n)

-- After running testProbs, some of the results are:
-- 
-- for n = 10000 
-- [2570,2429,2552,2449]
-- [2471,2500,2534,2495]
-- [2445,2522,2554,2479]
-- [2493,2498,2475,2534]
-- [2516,2525,2523,2436]
--
-- for n = 100000
-- [24862,24906,25212,25020]
-- [25148,25121,24908,24823]
-- [24972,25008,25018,25002]
-- [25155,24864,25020,24961]
-- [25067,24980,24888,25065]
--
-- As we can see, roughly the 25% of the floats belong to each of 
-- the 4 categories, proving that the random function works as expected
