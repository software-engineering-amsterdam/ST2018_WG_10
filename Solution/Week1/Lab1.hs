module Lab1 where
import Data.Char
import Data.List
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


-- Exercises --

-- 1 --

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


-- 2 --

-- This test will break on a list of length N if N becomes too large, since the number of
-- subsequences grows exponentially in N. A limit is set on N = 16, so the number of subsequences is
-- limited to 1024 using a take operation, making sure tests run reasonably fast.
--
-- A simple test testing on the length of lists doesn't specify anything aside from the number of
-- elements it should output.
-- This property is a mathematical fact that follows from the actual specification of the
-- function, which is to provide all subsequences of the given list,
-- and doesn't provide any additional information about the intended behavior.
-- Therefore this test also checks if the input is a list of unique elements, so the
-- output can be tested on uniqueness as well: if the input is a set of unique elements, the output
-- has to be a non-redundant set of unique sets.

sortunique :: (Eq a, Ord a) => [a] -> [a]
sortunique xs = mergesort xs
    where
        mergesort [] = []
        mergesort [x] = [x]
        mergesort xs = let half = length xs `quot` 2 in merge (mergesort $ take half xs) (mergesort $ drop half xs)
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
            | x == y = x:merge xs ys
            | x < y  = x:merge xs (y:ys)
            | x > y  = y:merge (x:xs) ys

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | x `elem` xs' = xs'
    | otherwise    = x:xs'
    where
        xs' = unique xs

testSequencesLength = quickCheck test
    where
        test :: [Integer] -> Bool
        test = (conjecture . sortunique . take 10)
        conjecture :: [Integer] -> Bool
        conjecture xs = (length . sortunique . map sortunique . subsequences) xs == 2 ^ length xs

-- 3 --

-- Again, this test breaks for a large N, now even faster than the previous exercise, so a
-- limit of N = 7 is set for reasonably fast testing.
-- This test does not provide a lot of information about the specification of the
-- function 'permutations', and only tests a property that follows from that specification.
-- Therefore this test is of limited value, and given the computational intensity it might be
-- a good idea to not test for this.

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

testPermutationsLength :: IO ()
testPermutationsLength = quickCheck test
    where
        test :: [Int] -> Bool
        test = (conjecture . sortunique . take 7)
        conjecture xs = (length . sortunique . permutations) xs == factorial (length xs)


-- 4 --
reversalPrimes :: [Integer]
reversalPrimes = [x | x <- takeWhile (<10000) primes, prime . reversal $ x]

reversalPrimesTest :: (NonNegative Integer) -> Bool
reversalPrimesTest (NonNegative n) = (elem n reversalPrimes) --> (elem (reversal n) reversalPrimes)

-- This test checks whether for any reversable prime, its reversal also exists.
-- Since all reversable primes of 4 or fewer digits are present, the reversal
-- must always exist in the list.
reversalPrimesQuickCheck :: IO ()
reversalPrimesQuickCheck = quickCheck reversalPrimesTest


-- 5 --

prime101Window :: Int -> [Integer]
prime101Window n = drop n (take (101 + n) primes)

prime101Sum :: [Integer]
prime101Sum = [sum (prime101Window x) | x <- [0..], prime (sum (prime101Window x))]

leastPrimeConsecutiveSum :: Integer
leastPrimeConsecutiveSum = head prime101Sum

-- a way of testing this would be by taking all consecutive primes less
-- than leastPrimeConsecutiveSum, however this would be functionally
-- equivalent to the function itself, so it might not be very useful.


-- 6 --

consecPrimesFrom2 :: Int -> [Integer]
consecPrimesFrom2 n = take n primes

consecPrimeConjecture :: Int -> Bool
consecPrimeConjecture n = prime (product (consecPrimesFrom2 n) + 1)

generateConsecPrimeCounterExamples :: [Int]
generateConsecPrimeCounterExamples = [x | x <- [1..], not (consecPrimeConjecture x)]

smallestConsecPrimeCounterExample = head generateConsecPrimeCounterExamples


-- 7 --

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


-- 8 --

-- xor helper function
xor :: Bool -> Bool -> Bool
xor = (/=)

-- helper function, so we can write 'x `exonerates` y'
exonerates :: Boy -> Boy -> Bool
exonerates b = not . accuses b


-- There are multiple ways of interpreting Jack's statement.
-- Interpretation A:
--   if either Matthew or Peter accuses x,
--   x is innocent. Otherwise, x is guilty.
-- Interpretation B:
--   if both Matthew and Peter accuse x,
--   x is innocent. Otherwise, x is guilty.
-- B failed to provide a good result, so the assumption was made
-- that A is correct.

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

accuses Jack boy = Matthew `exonerates` boy && Peter `exonerates` boy

accuses Arnold boy = (Matthew `accuses` boy) `xor` (Peter `accuses` boy)

accuses Carl boy = Arnold `exonerates` boy

accusers :: Boy -> [Boy]
accusers boy = [acc | acc<-boys, acc `accuses` boy]

data Solution a = None | Solution a | TooMany

solveguilty :: [([Boy], [Boy])]
solveguilty = filter (\(hs, gs) -> length gs == 1) [(hs, accusedby hs) | hs<-subgroups 3 boys]
    where
        subgroups :: Int -> [a] -> [[a]]
        subgroups n = filter ((== n) . length) . subsequences
        accusedby :: [Boy] -> [Boy]
        accusedby hs = filter (\b -> all (`accuses` b) hs) boys

honest :: Solution [Boy]
honest = toSolution solveguilty where
    toSolution [] = None
    toSolution [(h, _)] = Solution h
    toSolution _ = TooMany

guilty :: Solution [Boy]
guilty = toSolution solveguilty where
    toSolution [] = None
    toSolution [(_, g)] = Solution g
    toSolution _ = TooMany

puthonest :: IO()
puthonest = putsolution honest where
    putsolution None  = putStrLn "No solutions found"
    putsolution TooMany = putStrLn "No unique solution found"
    putsolution (Solution s) = putStrLn $ "  Honest:" ++ show s

putguilty :: IO()
putguilty = putsolution guilty where
    putsolution None  = putStrLn "No solutions found"
    putsolution TooMany = putStrLn "No unique solution found"
    putsolution (Solution s) = putStrLn $ "  Guilty:" ++ show s
