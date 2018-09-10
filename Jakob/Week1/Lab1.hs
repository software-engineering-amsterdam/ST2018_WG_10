module Lab1 where
import Data.List
import Data.Char
import Test.QuickCheck

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



-- Exercises


-- 1. (10 Min.) ✓
exercise2Before :: Int -> Int
exercise2Before n = sum [x^2 | x <- [1..n]]

exercise2After :: Int -> Int
exercise2After n = div ((n*(n+1)) * (2*n+1)) 6

exercise2Test :: (NonNegative Int) -> Bool
exercise2Test (NonNegative n) = exercise2Before n == exercise2After n

exercise2QuickCheck :: IO ()
exercise2QuickCheck = quickCheck exercise2Test

exercise3Before :: Int -> Int
exercise3Before n = sum [x^3 | x <- [1..n]]

exercise3After :: Int -> Int
exercise3After n = (div (n*(n+1)) 2)^2

exercise3Test :: (NonNegative Int) -> Bool
exercise3Test (NonNegative n) = exercise3Before n == exercise3After n

exercise3QuickCheck :: IO ()
exercise3QuickCheck = quickCheck exercise3Test


-- 2. (5 Min.) ✓
exercise4Test :: (NonNegative Int) -> Bool
exercise4Test (NonNegative n) = n <= 20 --> length (subsequences [1..n]) == 2^n

{--
  DISCLAIMER: I know that I am missing the constraint that we should use
  uniqueness of sets on Haskell lists. We did that though in our grouped solution.

  The test is somewhat difficult without the (-->) operator
  in `exercise4Test` owing to the fact that QuickCheck otherwise uses
  arbitrarily large integers to test which results in too high powers in `2^n`.
--}
exercise4QuickCheck :: IO ()
exercise4QuickCheck = quickCheck exercise4Test


-- 3. (10 Min.) ✓
fac = product . flip take [1..]

exercise5Test :: (NonNegative Int) -> Bool
exercise5Test (NonNegative n) = n <= 10 --> length (permutations intList) == fac (length intList)
                                where
                                  intList = [1..n]

{--
  DISCLAIMER: I know that I am missing the constraint that we should use
  uniqueness of sets on Haskell lists. We did that though in our grouped solution.

  The test is somewhat difficult without the (-->) operator
  in `exercise5Test` owing to the fact that QuickCheck otherwise uses
  arbitrarily large integers to test which results in too high factorials.
--}
exercise5QuickCheck :: IO ()
exercise5QuickCheck = quickCheck exercise5Test


-- 4. (10 Min.) ✓
reversalPrimes :: [Integer]
reversalPrimes = [x | x <- takeWhile (<10000) primes, prime . reversal $ x]

reversalPrimesTest :: (NonNegative Integer) -> Bool
reversalPrimesTest (NonNegative n) = (elem n reversalPrimes) --> (elem (reversal n) reversalPrimes)

reversalPrimesQuickCheck :: IO ()
reversalPrimesQuickCheck = quickCheck reversalPrimesTest


-- 5. (15 Min.) ✓
prime101Window :: Int -> [Integer]
prime101Window n = drop n (take (101 + n) primes)

prime101Sum :: [Integer]
prime101Sum = [sum (prime101Window x) | x <- [0..], prime (sum (prime101Window x))]

leastPrimeConsecutiveSum :: Integer
leastPrimeConsecutiveSum = head prime101Sum

-- I am still missing a possible test scenario


-- 6. (15 Min.) ✓
consecPrimesFrom2 :: Int -> [Integer]
consecPrimesFrom2 n = take n primes

consecPrimeConjecture :: Int -> Bool
consecPrimeConjecture n = prime (product (consecPrimesFrom2 n) + 1)

generateConsecPrimeCounterExamples :: [Int]
generateConsecPrimeCounterExamples = [x | x <- [1..], not (consecPrimeConjecture x)]

smallestConsecPrimeCounterExample = head generateConsecPrimeCounterExamples


-- 7. (30 Min.) ✓
luhnDouble :: Int -> Int
luhnDouble n = if doubled > 9 then doubled - 9 else doubled
               where
                 doubled = 2 * n

luhnDoubleFromChar :: Char -> Char
luhnDoubleFromChar x = intToDigit . luhnDouble . digitToInt $ x

alternateMap :: (a -> a) -> [a] -> [a]
alternateMap _ [] = []
alternateMap _ [x] = []
alternateMap f (x:xs:xss) = x : (f xs) : alternateMap f xss

sumString :: String -> Int
sumString x = sum [digitToInt n | n <- x]

retrieveLastDigitOfNumber :: Integer -> Int
retrieveLastDigitOfNumber n = digitToInt . last . show $ n

luhn :: Integer -> Bool
luhn n = mod (nonCheckSum + checkDigit) 10 == 0
         where
           nonCheckSum = sumString (alternateMap luhnDoubleFromChar (show n))
           checkDigit  = retrieveLastDigitOfNumber n

numberInRange :: Int -> (Int, Int) -> Bool
numberInRange n (k,l) = elem n [k..l]

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = (iin == 34 || iin == 37) && luhn n
                      where
                        iin = read . take 2 . show $ n :: Int

isMaster :: Integer -> Bool
isMaster n = (numberInRange iin1 (2221,2720) || numberInRange iin2 (51,55)) && luhn n
             where
               iin1 = read . take 4 . show $ n :: Int
               iin2 = read . take 2 . show $ n :: Int

isVisa :: Integer -> Bool
isVisa n = iin == 4 && luhn n
           where
             iin = read . take 1 . show $ n :: Int

-- Generates the check digit for a given card number with it still missing at the end
generateLuhnCheckDigit :: Integer -> Int
generateLuhnCheckDigit n = mod (nonCheckSum * 9) 10
                           where
                             nonCheckSum = sumString (alternateMap luhnDoubleFromChar (show n))

-- A custom generator that generates arbitrary credit card numbers with the check digit missing
cardNumber :: Gen Integer
cardNumber = choose (1000000000, 9999999999)

-- Calculates the check digit for a given card number and then checks if that passes the Luhn algorithm
luhnTest :: Integer -> Bool
luhnTest n = luhn concatCardNumber
             where
               concatCardNumber = read (show n ++ show (generateLuhnCheckDigit n)) :: Integer

-- Tests the custom generator with the written test
luhnQuickCheck :: IO ()
luhnQuickCheck = quickCheck (forAll cardNumber luhnTest)


-- 8. (60 Min.)

-- DISCLAIMER:
-- I unfortunately did not finish this, we did though in the grouped solution

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

accuses :: Boy -> Boy -> Bool
accuses Matthew Matthew = False
accuses Matthew Carl    = False
accuses Matthew _       = True
accuses Peter Matthew   = True
accuses Peter Jack      = True
accuses Peter _         = False
accuses Jack boy        = (not (accuses Matthew boy)) && (not (accuses Peter boy))
accuses Arnold boy      = (accuses Matthew boy) `xor'` (accuses Peter boy)
accuses Carl boy        = not (accuses Arnold boy)

accusers :: Boy -> [Boy]
accusers b = [x | x <- boys, accuses x b]

constructTruthCombinations [x,xs,xss,xsss,xssss] = [(x,True),(xs,True),(xss,True),(xsss,False),(xssss,False)]

-- truthCombinations :: [(Boy, Bool)] -> [Boy]
truthCombinations []                                = []
truthCombinations ((boy, truth):xs) | truth == True = [[x | x <- boys, accuses boy x]] ++ truthCombinations xs
                                    | otherwise     = [[x | x <- boys, not (accuses boy x)]] ++ truthCombinations xs

-- guilty :: [Boy]
guilty = guiltyBoys
         where
           guiltyBoys   = [truthCombinations b | b <- combinations]
           combinations = [constructTruthCombinations x | x <- permutations boys]

-- honest :: [Boy]
