module Lab4 where

import           Data.List
import           SetOrd
import           System.Random
import           Test.QuickCheck


-- Exercises

-- 1)




-- 2) (15 Min.)

-- From scratch
genRandIntBetween1And10 :: IO Int
genRandIntBetween1And10 = randomRIO (1 :: Int, 10 :: Int)

setGenScratch :: IO (Set Int)
setGenScratch = do
  length <- genRandIntBetween1And10
  list <- sequence (replicate length genRandIntBetween1And10)
  return (list2set list)

-- With QuickCheck
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = fmap list2set arbitrary


-- 3)




-- 4)




-- 5)




-- 6)




-- 7)




-- 8)
