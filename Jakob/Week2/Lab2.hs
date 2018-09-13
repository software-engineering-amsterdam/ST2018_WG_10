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

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- Exercises

-- 1)

-- 2) (20 Min.)

pythagorean :: (Integer, Integer, Integer) -> Bool
pythagorean (a,b,c) = a^2 + b^2 == c^2

triangleImpossible :: (Integer, Integer, Integer) -> Bool
triangleImpossible (a,b,c) = (a + b) < c

tripleIsEqual :: (Integer, Integer, Integer) -> Bool
tripleIsEqual (x,y,z) = (x == y) && (y == z) && (x == z)

twoInTripleAreEqual :: (Integer, Integer, Integer) -> Bool
twoInTripleAreEqual (x,y,z) = x == y || y == z || x == z

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | triangleImpossible (a,b,c)  = NoTriangle
               | tripleIsEqual (a,b,c)       = Equilateral
               | pythagorean (a,b,c)         = Rectangular
               | twoInTripleAreEqual (a,b,c) = Isosceles
               | otherwise                   = Other

{-
Here goes the test report
-}

-- 3)

-- 4)

-- 5)

-- 6)

-- 7)
