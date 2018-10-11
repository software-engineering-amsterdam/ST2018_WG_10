module Lab6 where

import Data.Maybe
import Lecture6 hiding
    ( composites
    , exM
    , expM
    , mrComposite
    , primeMR
    , primeTestF
    , primeTestsF
    )
import System.IO.Unsafe
import System.Random
import System.Timeout
import Test.QuickCheck



-- Exercise 1. -- (1 hour)
-- =========== --
expM :: Integer -> Integer -> Integer -> Integer
expM x y = rem (x ^ y)

sqrM :: Integer -> Integer -> Integer
sqrM x m = rem (x * x) m

exM :: Integer -> Integer -> Integer -> Integer
exM _ _ 1 = 0
exM _ 0 _ = 1
exM x 1 m = rem x m
exM x y m =
    let oddf = x ^ (rem y 2)
        sq = sqrM (exM x (quot y 2) m) m
     in multM oddf sq m



-- Exercise 2. -- (1 hour)
-- =========== --
-- Test equivalence
prop_ExpEquiv (NonNegative x) (NonNegative y) (Positive m) =
    exM x y m == rem (x ^ y) m

-- Performance test
randomBigIntSeq :: [IO Integer]
randomBigIntSeq = map (\n -> randomRIO (10 ^ n, 10 ^ (n + 1))) [1 ..]

-- find the maximum exponent n = 2^k for which exf x (2^n) is reasonably
-- fast, and report it back. 
testExM :: (Integer -> Integer -> Integer -> Integer) -> IO ()
testExM exf = do
    test <- testExM' exf 1
    putStrLn $ "Test result: " ++ show test
  where
    testExM' exf n = do
        m <- (\x -> exf 10 x (2 ^ (n + 1))) <$> (randomBigIntSeq !! n)
        r <- timeout (10 ^ 5) $ (m `seq` return ())
        if isJust r
            then testExM' exf (n * 2)
            else return n

-- Result: 
--   >>> testExM exM
--   > Test result: 8192
--
--   >>> testExM expM
--   > Test result: 8


-- Exercise 3. -- (20 min.)
-- =========== --
composites :: [Integer]
composites = [x | x <- [1 ..], not $ prime x]

-- Exercise 4. -- (40 min.)
-- =========== --
primeTestF :: Integer -> IO Bool
primeTestF n = do
    a <- randomRIO (2, n - 1) :: IO Integer
    return (exM a (n - 1) n == 1)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
    as <- sequence $ fmap (\_ -> randomRIO (2, n - 1)) [1 .. k]
    return (all (\a -> exM a (n - 1) n == 1) as)

foolPrime :: Int -> IO Integer
foolPrime k = test' k 0
  where
    test' k n = do
        let c = composites !! n
        fooled <- primeTestsF k c
        if fooled
            then return c
            else test' k (n + 1)

testOneToThree :: IO [()]
testOneToThree =
    sequence $
    map (\x -> do
             prime <- foolPrime x
             putStrLn $ show prime)
        [1 .. 3]

-- Exercise 5. --
-- =========== --
carmichael :: [Integer]
carmichael =
    [ (6 * k + 1) * (12 * k + 1) * (18 * k + 1)
    | k <- [2 ..]
    , prime (6 * k + 1)
    , prime (12 * k + 1)
    , prime (18 * k + 1)
    ]

foolPrime' :: Int -> IO Integer
foolPrime' k = test k 0
  where
    test k n = do
        let c = carmichael !! n
        fooled <- primeTestsF k c
        if fooled
            then return c
            else test k (n + 1)

-- Exercise 6. --
-- =========== --
mrComposite :: Integer -> Integer -> Bool
mrComposite x n =
    let (r, s) = decomp (n - 1)
        fs = takeWhile (/= 1) (map (\j -> exM x (2 ^ j * s) n) [0 .. r])
     in exM x s n /= 1 && last fs /= (n - 1)

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do
    a <- randomRIO (2, n - 1) :: IO Integer
    if exM a (n - 1) n /= 1 || mrComposite a n
        then return False
        else primeMR (k - 1) n

foolMrPrime :: Int -> IO Integer
foolMrPrime k = test k 0
  where
    test k n = do
        let c = carmichael !! n
        fooled <- primeMR k c
        if fooled
            then return c
            else test k (n + 1)

-- primeMR isn't easily fooled.


-- Exercise 7. --
-- =========== --
mersenneCandidates = map (\x -> 2 ^ x - 1) primes

mersennePrimes k = mprs 0
  where
    mprs n = do
        let c = mersenneCandidates !! n
        isMRPrime <- primeMR k c
        if isMRPrime
            then do
                putStrLn $ show c
                putStrLn $ "C is mersenne: " ++ show (c `elem` mers')
            else return ()
        mprs (n + 1)
    mers' = map mers [1 .. 25]
