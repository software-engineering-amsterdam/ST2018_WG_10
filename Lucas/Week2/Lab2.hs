
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


--1
numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound ub list = sum $ map (\a -> 1) $ filter (< ub) list
                  
testProbs :: Int -> IO [Int]
testProbs n = do
                  rdm <- probs n
                  let res1 = numTimesFound 0.25 rdm
                  let res2 = numTimesFound 0.5 rdm - res1
                  let res3 = numTimesFound 0.75 rdm - res2 - res1
                  let res4 = numTimesFound 1.0 rdm - res3 - res2 - res1
                  return [res1,res2,res3,res4]


--2
triangleTest :: Integer -> Integer -> Integer -> Shape
triangleTest a b c = do
                        let s = (fromIntegral(a + b + c))/2
                        let sq = sqrt (s*(s-fromIntegral a)*(s-fromIntegral b)*(s-fromIntegral c))
                        if (a <= 0) || (b <= 0) || (c <= 0) || (sq <= 0)
                            then NoTriangle
                            else if (a == b) && (b == c)
                                 then Equilateral
                                 else if a^2 + b^2 == c^2
                                      then Rectangular
                                      else if ((a == b) || (a == c) || (c == b)) && ((a+b > c) && (a+c > b) && (c+b > a))
                                           then Isosceles
                                           else Other

--3

--Derangements

isDerangement :: [Int] -> [Int] -> Bool
isDerangement [] _ = True
isDerangement l1 l2 = if head l1 /= head l2
                         then isDerangement (tail l1) (tail l2)
                         else False


deran :: [Int] -> [[Int]]
deran d = filter (\n -> isDerangement d n) (permutations d)


--ROT13

rot13Word :: [Char] -> [Char]
rot13Word w = map rot13 w

rot13 :: Char -> Char
rot13 c
  | isAlpha c = chr (if_ (toLower c <= 'm') (+) (-) (ord c) 13)
  | otherwise = c

if_ :: Bool -> a -> a -> a
if_ True x _ = x
if_ False _ y = y


--BRITISH FORMAT CHECKER
ibans = ["GB24BKEN10000031510604", "GB27BOFI90212729823529", "GB17BOFS80055100813796", "GB92BARC20005275849855", "GB66CITI18500812098709", "GB15CLYD82663220400952", "GB26MIDL40051512345674", "GB76LOYD30949301273801"]

iban :: String -> Bool
iban ib = do
            let lett = map (\x -> if (x < 0) then x+17 else x) (map (\x -> if (x > 0) then x+10 else x) ((map (\x -> ord (toUpper x) - ord 'A') (drop 4 ib ++ take 4 ib))))            
            let cnv = read (concat $ map (show) lett) :: Integer
            if (rem cnv 97) == 1
                        then True
                        else False