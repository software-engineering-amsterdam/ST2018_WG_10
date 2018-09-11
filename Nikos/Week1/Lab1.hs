
module Lab1 where
import Data.List
import Test.QuickCheck   

-- LAB 1 -- 

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Haskell tutorials (~15 hours) --


-- Excercise 1  (2 hours) --

sqrAdd :: Integer -> Integer
sqrAdd 0 = 0
sqrAdd n = n*n + sqrAdd(n-1)

mysqrAdd :: Integer -> Integer
mysqrAdd n = n*(n+1)*(2*n+1) `div` 6

cubeAdd :: Integer -> Integer
cubeAdd 0 = 0
cubeAdd n = n*n*n + cubeAdd(n-1)

mycubeAdd :: Integer -> Integer
mycubeAdd n = (n*(n+1) `div` 2)^2

mysqrAddTest :: Integer -> Bool
mysqrAddTest = \x -> x>=0 --> sqrAdd x == mysqrAdd x

mycubeAddTest :: Integer -> Bool
mycubeAddTest = \x -> x>0 --> cubeAdd x == mycubeAdd x

-- Excercise 4 (1 hour) --

reversePrimes :: [Integer]
reversePrimes = takeWhile (< 10000) (filter (prime . reversal) primes)

reversePrimesTest :: Integer -> Bool
reversePrimesTest = \x -> (x>=0 && x <= 10000) && (x `elem` reversePrimes) --> ((reversal x) `elem` reversePrimes && (reversal x) `elem` primes)

-- Excercise 5 (1,5 hour) --

select101Primes :: Int -> Integer
select101Primes n = sum . take 101 $ drop n primes

sumPrime :: Int -> Integer
sumPrime n = if (prime . select101Primes $ n)
    then select101Primes n
    else sumPrime (n+1)

findSumPrime :: Integer
findSumPrime = sumPrime 0
