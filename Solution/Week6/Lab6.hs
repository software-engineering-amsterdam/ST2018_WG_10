module Lab6 where

import           Data.List
import           Data.Maybe
import           Lecture6        hiding (composites, exM, expM, mrComposite,
                                  primeMR, primeTestF, primeTestsF)
import           System.Random
import           System.Random
import           System.Timeout
import           Test.QuickCheck


-- Exercises

-- 1)

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


-- 2)

-- Test equivalence
prop_ExpEquiv (NonNegative x) (NonNegative y) (Positive m) =
    exM x y m == rem (x ^ y) m

-- Performance test
randomBigIntSeq :: [IO Integer]
randomBigIntSeq = map (\n -> randomRIO (10 ^ n, 10 ^ (n + 1))) [1 ..]

-- Find the maximum exponent n = 2^k for which exf x (2^n) is reasonably fast, and report it back.
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

test_exM = testExM exM
test_expM = testExM expM

{-
  Result:

  >>> test_exM
  > Test result: 8192 -- (or anything higher depending on the computer ran on)

  >>> test_expM
  > Test result: 8

  The output represents the maximum size of the input integer in bits for which
  it performs reasonably fast (under a 10th of a second).
  Thus, it can be assessed the `exM` implementation can handle thousand times
  the size of the `expM` one.
-}


-- 3)

composites :: [Integer]
composites = [x | x <- [2..], not . prime $ x]


-- 4)

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


-- 5)

-- The given function does not produce the first few Carmichael numbers
carmichael :: [Integer]
carmichael = [(6*k+1)*(12*k+1)*(18*k+1) | k <- [2..],
                                          prime (6*k+1),
                                          prime (12*k+1),
                                          prime (18*k+1)]

-- Outputs the first Carmichael number that fools Fermat's primality test for a given k
foolPrime' :: Int -> IO Integer
foolPrime' k = test k 0
  where
    test k n = do
        let c = carmichael !! n
        fooled <- primeTestsF k c
        if fooled
            then return c
            else test k (n + 1)

{-
 It can be observed that with a random coprime of any Carmichael number,
 Fermat's primality test yields `True` although all of the Carmichael numbers
 are actually composites. Thus, it is easily fooled.
-}


-- 6)

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

-- Outputs the first Carmichael number that fools the Miller-Rabin primality check for a given k
foolMrPrime :: Int -> IO Integer
foolMrPrime k = test k 0
  where
    test k n = do
        let c = carmichael !! n
        fooled <- primeMR k c
        if fooled
            then return c
            else test k (n + 1)

{-
  `primeMR` isn't easily fooled. Much larger carmichael numbers are found e.g.:
  >>> foolMrPrime 3
  > 3470513474822998969

  Larger values of k, e.g. 8, result in infeasible computation times.
-}


-- 7)

-- The actual Mersenne primes based on the `mers` function
mersennePrimes :: [Integer]
mersennePrimes = map mers [1..25]  -- the first 25 Mersenne primes are accessible via the `mers` function

-- Potential Mersenne primes based on the first n primes
potentialLargeMersennePrime :: Int -> [Integer]
potentialLargeMersennePrime n = map (\p -> 2^p - 1) (take n primes)

-- Test which of the potential Mersenne primes are actual Mersenne primes
actualLargeMersennePrime :: Int -> [Integer]
actualLargeMersennePrime n = filter (\p -> elem p mersennePrimes) (potentialLargeMersennePrime n)

{-
  Test which of the potential Mersenne primes are actual Mersenne primes.
  The input n changes how many potential Mersenne primes will be tested.
  The output triple corresponds to the following pattern:
  (potential_mersenne_number, is_prime, is_actual_mersenne_number)
-}
largeMersennePrimes :: Int -> IO [(Integer, Bool, Bool)]
largeMersennePrimes n = do
  let potentials = potentialLargeMersennePrime n
  resultsMR <- sequence (map (\m -> primeMR 2 m) potentials)
  let resultsMersenne = map (\p -> elem p mersennePrimes) potentials
  return (zip3 potentials resultsMR resultsMersenne)

-- Test if prime `potentialLargeMersennePrime` are all Mersenne primes as well
test_filteredPotentialLargeMersennePrimes :: Int -> Bool
test_filteredPotentialLargeMersennePrimes n = filter prime (potentialLargeMersennePrime n) == actualLargeMersennePrime n

{-
  It can be shown that not every potential Mersenne prime calculated with
  the given formula 2^p-1 is actually one.
  Many of them like 11 are not even outputting prime numbers in the first place.
  In fact, it seems that every `potentialLargeMersennePrime` that is actually
  a prime number is also a valid Mersenne prime. This is partially proven with
  the function `test_filteredPotentialLargeMersennePrimes`.
  Finally, the function `largeMersennePrimes` returns n of the first Mersenne
  primes together with the two information if they are a valid prime via
  the Miller-Rabin-Test and if they are part of the `mersennePrimes`.
-}
