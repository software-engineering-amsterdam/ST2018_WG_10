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


-- 4)

coprimesOfN :: Integer -> [Integer]
coprimesOfN x = [pc | pc <- [2..], coprime x pc]

fermat :: Integer -> Integer -> Bool
fermat p a = (a ^ (p - 1)) `mod` p == 1 `mod` p

foolFermat :: [Integer]
foolFermat = nub [c | c <- composites, x <- take 100 (coprimesOfN c), fermat c x]

smallestFoolFermat :: Integer
smallestFoolFermat = head foolFermat


-- 5)

carmichael :: [Integer]
carmichael = [(6*k+1)*(12*k+1)*(18*k+1) | k <- [2..],
                                          prime (6*k+1),
                                          prime (12*k+1),
                                          prime (18*k+1)]

-- The given function does not produce the first few Carmichael numbers.
-- Taken from here: http://oeis.org/A002997
carmichaelReal :: [Integer]
carmichaelReal = [561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745, 63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821, 334153, 340561, 399001, 410041, 449065, 488881]

prop_foolFermatWithCarmichael :: Positive Int -> Bool
prop_foolFermatWithCarmichael (Positive n) = let cmAmount = 3 in
                                             length [True | cm <- (take cmAmount carmichaelReal), fermat cm (toInteger n)] == cmAmount

check_foolFermatWithCarmichael :: IO ()
check_foolFermatWithCarmichael = verboseCheck prop_foolFermatWithCarmichael

{-
 Fermat's primality test is unfortunately very slow to be ran on many Carmichael
 numbers. I used QuickCheck to test if Fermat's primality test indeed holds
 for the first three Carmichael numbers with random base numbers to test
 which get provided by QuickCheck. Using more than the first three Carmichael
 numbers increases the runtime of the `check_foolFermatWithCarmichael` to
 a tremendous amount owing to the fact that all of them are large integers.
-}

-- 6)


-- Find a `d` in the Miller-Rabin-Test formula
mrtD :: Integral a => a -> a
mrtD n | odd n     = head [d | d <- [1,3..], j <- [1..10], (n - 1) == (d * (2^j))]
       | otherwise = -1  -- no even number can be a prime

-- Generate any base `a` for the Miller-Rabin-Test formula
mrtBase :: (Num a, Enum a) => a -> IO a
mrtBase n = do
  let set = [2..(n-2)]  -- set construction given by the formula
  index <- randomRIO (0 :: Int, (length set) - 1 :: Int)
  return (set !! index)

mrtRun :: Integral a => a -> IO Bool
mrtRun n | even n    = return False  -- no even number can be a prime
         | otherwise = do
           let d = mrtD n
           a <- mrtBase n
           return ((a^d `mod` n) == (1 `mod` n))

-- Run the Miller-Rabin-Test a few thousand times with varying, random bases to be more certain that it is correct
mrt :: Integral a => a -> IO Bool
mrt n = do
  runs <- sequence (replicate 3000 (mrtRun n))  -- 3000 runs with differing bases are performed to increase the certainty
  return (any (== True) runs)  -- as long as any of the runs yielded a `True` result, the whole MRT considers the input to be prime

-- Test the first n Carmichael numbers via the Miller-Rabin-Test
test_mrtCarmichael :: Int -> IO [(Integer, Bool)]
test_mrtCarmichael n = do  -- the `n` represents which of the first n Carmichael numbers should be tested
  let cm = take n carmichaelReal
  results <- sequence (map mrt cm)
  return (zip (map id cm) results)  -- `zip` the results with the input numbers to see which ones yielded which result

-- Test if the implemented Miller-Rabin-Test does produce false positives for known composites
test_mrtComposite :: Int -> IO [(Integer, Bool)]
test_mrtComposite n = do
  let comps = take n composites
  results <- sequence (map mrt comps)
  return (zip (map id comps) results)  -- `zip` the results with the input numbers to see which ones yielded which result

-- Taken from here: https://oeis.org/A001262
strongPseudoprimesToBase2 :: [Integer]
strongPseudoprimesToBase2 = [2047, 3277, 4033, 4681, 8321, 15841, 29341, 42799, 49141, 52633, 65281, 74665, 80581, 85489, 88357, 90751, 104653, 130561, 196093, 220729, 233017, 252601, 253241, 256999, 271951, 280601, 314821, 357761, 390937, 458989, 476971, 486737]

test_mrtStrongPseudoprimesToBase2 :: Int -> IO [(Integer, Bool)]
test_mrtStrongPseudoprimesToBase2 n = do
  let pseudos = take n strongPseudoprimesToBase2
  results <- sequence (map mrt pseudos)
  return (zip (map id pseudos) results)  -- `zip` the results with the input numbers to see which ones yielded which result


{-
  Carmichael numbers are still not detectable as composites, even with the
  Miller-Rabin primality check (`mrt`).
  However, it yields a result that marks Carmichael numbers as composites
  more often varying with the chosen base (a).
  Therefore, the approach was chosen to run the Miller-Rabin primality check
  3000 times in a row with differing bases (a) to determine if any run
  yielded a positive result in terms of the question if the input is a prime.
  Additionally, with composites this also does not always yield `False`
  which shows that the test does produce false positives.
  Precisely, other strong pseudo-primes like the number 121 will also be
  detected by the Miller-Rabin primality check (`mrt`) as prime although
  this number is in fact not a prime.
  Finally the `mrt` function was run on known strong pseudo-primes to ascertain
  the described hypothesis.
  More on strong pseudo-primes: https://en.wikipedia.org/wiki/Strong_pseudoprime
-}


-- 7)
