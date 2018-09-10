module Exercise1 where
import Data.List
import Debug.Trace
import Test.QuickCheck
import Lab1

--  1. (~20 minutes)
-- =======

sqsum :: Int -> Int
sqsum n = n * (n + 1) * (2 * n + 1) `quot` 6


cubesum :: Int -> Int
cubesum n = (n * (n + 1) `quot` 2) ^ 2

-- uses QuickCheck's NonNegative data type to ensure non-negative integers
testsqsum = quickCheck test where
    test :: NonNegative Int -> Bool
    test (NonNegative n) = sum [m^2 | m <- [1..n]] == sqsum n

testcubesum = quickCheck test where
    test :: NonNegative Int -> Bool
    test (NonNegative n) = sum [m^3 | m <- [1..n]] == cubesum n


--  2. (~30 minutes)
-- ======= 

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
    
--  3. (~10 minutes)
-- ====

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





--  4. (~20 minutes)
-- ============

reversePrimes :: [Integer]
reversePrimes = [n | n <- takeWhile (<10000) primes, prime . reversal $ n]


-- This test checks whether for any reversable prime, its reversal also exists. 
-- Since all reversable primes of 4 or fewer digits are present, the reversal 
-- must always exist in the list.
testReversePrimes = quickCheck test where
    test :: NonNegative Int -> Bool
    test (NonNegative n) = n <= length reversePrimes --> prime p && reversal p `elem` reversePrimes 
        where p = reversePrimes !! n





--  5. (~30 minutes)
-- ====

leastPrimeConsecutiveSum :: Integer
leastPrimeConsecutiveSum = head . filter prime . map sum101PrimesFrom $ [1..] where
    sum101PrimesFrom :: Int -> Integer
    sum101PrimesFrom n = sum . take 101 . drop n $ primes

-- a way of testing this would be by taking all consecutive primes less 
-- than leastPrimeConsecutiveSum, however this would be functionally 
-- equivalent to the function itself, so it might not be very useful.  


--  6. (~ 10 minutes)
-- ====

conjecture n = prime . (+ 1) . product . take n $ primes

leastRefutal = take (head refutals) primes where
    refutals = [n | n <- [1..], not . conjecture $ n]

--  7. (~30 minutes)
-- ====

infixl 1 &
(&) :: a -> (a -> b) -> b
(&) a f = f a

luhn :: Integer -> Bool
luhn n = (\m -> (m `mod` 10) == 0) $ sum $ map toSingleDig $ double2nds $ digits $ n
    where
        digits :: Integer -> [Integer]
        digits n = [read [d] | d<-reverse . show $ n]
        double2nds :: [Integer] -> [Integer]
        double2nds digits = [a * b|(a, b)<-zip (cycle [1,2]) digits]
        toSingleDig :: Integer -> Integer
        toSingleDig n | n > 9     = n - 9
                      | otherwise = n

--  8. (~1 hr)
-- ====

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
    putsolution (Solution s) = putStrLn $ "  Honest:" ++ show s

-- run it all
main :: IO()
main = do
    putStrLn "Exercise 1:" 
    testsqsum
    testcubesum

    putStrLn "Exercise 2:"
    testSequencesLength

    putStrLn "Exercise 3:"
    testPermutationsLength

    putStrLn "Exercise 4:"
    testReversePrimes

    putStrLn "Exercise 5:"
    putStrLn "Least sum of 101 consecutive primes:"
    putStrLn $ "  " ++ show leastPrimeConsecutiveSum

    putStrLn "Exercise 6:"
    putStrLn $ "  Least refutal: " ++ show leastRefutal

    putStrLn "Exercise 8:"
    putguilty
    puthonest
