module Lab2 where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           System.Random
import           Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q



-- Excercise 1

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

-- chisquaredUniform takes a list of categories, and checks if the categories
-- are distributed uniformly, by means of returning the chi squared statistic for the observed
-- distribution, tested against the uniform distribution.
chisquaredUniform :: Ord a => [a] -> Float
chisquaredUniform obs = sum $ map (\o -> (o - e)^2 / e) freqs
    where
        n = fromIntegral $ length obs
        freqs = map (fromIntegral . length) . group . sort $ obs
        ncats = fromIntegral $ length freqs
        e :: Float
        e = n / ncats

-- This test checks a random generator of type IO [Float] and tests whether
-- the returned distribution satisifies a uniform distribution over the bins:
-- [0, 0.25), [0.25, 0.5), [0.5, 0.75), [0.75, 1]
-- This is done using a chi squared uniformity test.
-- The test statistic is considered significant for values > 16.72, which
-- corresponds to p > 0.1% for 3 degrees of freedom. Consequently, this test
-- has a false positive rate of approximately 0.1%.
--
-- Example outputs:
--   >>> uniformCheck $ probs 10000
--   > No significant deviation found. (Chi sq. = 1.2269, p > 0.1%)
--
-- For a biased distribution:
--   >>> uniformCheck $ fmap (map (^2)) $ probs 10000
--   > Found deviation from expected value: [4991,2056,1660,1293] is not uniform, (Chi sq. = 3425.8667, p < 0.1%)
--
uniformCheck :: IO [Float] -> IO ()
uniformCheck gen = do
    obs <- gen
    bins <- pure $ map (truncate . (*4)) obs
    counts <- pure $ foldr (\n cs -> take n cs ++ (cs !! n)+1:(drop (n + 1) cs)) [0,0,0,0] bins
    chisq <- pure $ chisquaredUniform bins
    putStrLn $ show $ take 100 counts
    if chisq < 16.27 then
        putStrLn $ "No significant deviation found. (Chi sq. = " ++ show chisq ++ ", p > 0.1%)"
    else
        putStrLn $ "Found deviation from expected value: " ++ show counts ++ " is not uniform, (Chi sq. = " ++ show chisq ++ ", p < 0.1%)"

-- Testing the probs function
-- Example output:
--   >>> probsCheck
--   > No significant deviation found. (Chi sq. = 1.2269, p > 0.1%)
probsCheck = uniformCheck $ probs 10000


-- Exercise 2

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

-- Used paper "ANALYZING THE TRIANGLE PROBLEM" to get the special cases for this problem. I tested the function with help of these special cases.
-- The paper is written by Collard & Company


-- Excercise 3

data PropDef = PropDef String (Integer -> Bool)

instance Eq PropDef
    where
        (PropDef name _) == (PropDef nameb _) = name == nameb

instance Show PropDef
    where
        show (PropDef name _) = name

instance Ord PropDef
    where
        (PropDef _ p) <= (PropDef _ q) = not $ stronger [1..10] p q

forall = flip all

stronger :: [Integer] -> (Integer -> Bool) -> (Integer -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs = flip (stronger xs)

prop0 = PropDef "even x" (\x -> even x)
prop1 = PropDef "even x && x > 3" (\x -> even x && x > 3)
prop2 = PropDef "even x || x > 3" (\x -> even x || x > 3)
prop3 = PropDef "even x && x > 3 || even x" (\x -> even x && x > 3 || even x)


sortedProp = sort [prop0, prop1, prop2, prop3]

isSorted :: ((Integer -> Bool) -> (Integer -> Bool) -> Bool) -> [(Integer -> Bool)] -> Bool
isSorted _ []       = True
isSorted _ [_]      = True
isSorted c (x:y:xs) = c y x && isSorted c (y:xs)

-- test if list of PropDef items is ordered by stronger
checkSorted = isSorted (stronger [1..10]) $ map (\(PropDef _ p) -> p) sortedProp


-- Excercise 4

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs /= ys && isPermutation' xs ys where
    isPermutation' [] [] = True
    isPermutation' [] _ = False
    isPermutation' (x:xs) ys = fromMaybe False $ do
        n <- elemIndex x ys
        ys' <- Just $ take n ys ++ drop (n + 1) ys
        return $ isPermutation' xs ys'


-- Precondition: {} any two lists in the domain should work.
-- Postcondition: {\x -> x --> xs is a permutation of ys }

-- Let xs and ys be the lists we want to test
-- Some properties:
--
--   1. isPermutation xs ys <==> (length xs == length ys)
--   2. isPermutation xs ys <==> (x `elem` xs <--> x `elem` ys), ∀ x
--   3. isPermutation xs ys <==> (xs /= ys)
--   4. isPermutation xs ys <==> (x `elem` xs <--> x `elem` ys) && xs /= ys,  ∀ x
--
-- These properties can't be fully ordered by strength.
-- Property 2 is stronger than 1, because we may assume that all elements in a list are unique.
-- Property 3 is not stronger or weaker than 1 or 2, examples are (xs, ys) = ([1,2,3], [1,2,3]) and (xs, ys) = ([1,2], [1,2,3])
-- Property 4 is the strongest property, because it's a conjunction of 2. and 3.
--
--
-- A naive implementation of property 4 would have the signature
-- prop_isPermutation :: [Integer] -> [Integer] -> Bool
--
-- The problem here is that the probability that quickCheck provides two permutations is very slim,
-- so the left side of the disjunction will never be tested.
-- An Arbitrary that has a greater chance of generating useful lists is necessary.


prop_isPermutation (ArbitraryPermutation (xs, ys)) = unique xs && unique ys ==>
        conditionL && conditionR || not conditionL && not conditionR
    where
        conditionL = isPermutation xs ys
        conditionR = all (`elem` ys) xs && all (`elem` xs) ys && xs /= ys
        unique xs = (length . group . sort $ xs) == length xs

newtype ArbitraryPermutation = ArbitraryPermutation ([Integer], [Integer]) deriving (Eq, Ord, Show)

instance Arbitrary ArbitraryPermutation
    where
        arbitrary = do
            self <- arbitrary
            other <- oneof [pure self, shuffle self]
            return $ ArbitraryPermutation (self, other)
            where
                arbitraryPermute xs = do
                    mutationDrop <- choose (0, 1)
                    permutation <- shuffle xs
                    mutated <- shuffle $ drop mutationDrop xs
                    return mutated

                shuffle :: [a] -> Gen [a]
                shuffle = shuffle' []
                    where
                        shuffle' :: [a] -> [a] -> Gen [a]
                        shuffle' acc [] = pure acc
                        shuffle' acc [x] = pure (x:acc)
                        shuffle' acc xs = do
                            idx <- choose(0, length xs - 1)
                            h <- pure $ take idx xs
                            t <- pure $ drop (idx + 1) xs
                            x <- pure $ xs !! idx
                            shuffle' (x:acc) (h ++ t)

                mutate :: [a] -> Gen [a]
                mutate xs = (liftM $ (`drop` xs)) (choose (0, length xs))


-- Exercise 5

isDerangement :: (Eq a, Integral a) => [a] -> [a] -> Bool
isDerangement [] []         = True
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys

deran :: (Eq a, Integral a) => [a] -> [[a]]
deran [] = []
deran xs = filter (isDerangement xs) (permutations xs)

subfact :: (Eq a, Num a) => a -> a
subfact 0 = 1
subfact 1 = 0
subfact n = (n - 1) * (subfact (n - 2) + subfact (n - 1))

derangementTest :: Int -> Property
derangementTest n = (n >= 2 && n <= 7) ==> length (deran [1..n]) == subfact n

derangementQuickCheck = quickCheck derangementTest

-- [JAKOB Insert text]


-- Exercise 6

-- For convenience, 'ord' and 'chr' are omitted in the specification.
--
-- Specification for helper function rot13char:
--   rot13char :: Char -> Char
--   { λc -> isLower    c } λc -> rot13char c  { λc' -> c' == 'a' + ((c - 'a' + 13) `mod` 26) }
--   { λc -> isUpper    c } λc -> rot13char c  { λc' -> c' == 'A' + ((c - 'A' + 13) `mod` 26) }
--   { λc -> isNonAlpha c } λc -> rot13char c  { λc' -> c' == c) }

rot13char :: Char -> Char
rot13char c
    | isLower c = chr $ shift13 (ord 'a') (ord c)
    | isUpper c = chr $ shift13 (ord 'A') (ord c)
    | otherwise = c
    where
        shift13 zero c = zero + ((c - zero + 13) `mod` 26)


-- Specification of the rot13 function:
-- { λcs     → cs = [] } λcs     → rot13 cs   { λcs'      → cs' == [] }
-- { λ(c:cs) → ⊤       } λ(c:cs) → rot13 c:cs { λ(c':cs') → c' = rot13char c ∧ cs' = rot13 cs }

rot13 :: String -> String
rot13 []     = []
rot13 (c:cs) = (rot13char c:rot13 cs)


prop_rot13charLower c    = isLower c ==> ((\c' -> ord c' == (ord 'a') + (((ord c) - (ord 'a') + 13) `mod` 26)) $ rot13char c)
prop_rot13charUpper c    = isUpper c ==> ((\c' -> ord c' == (ord 'A') + (((ord c) - (ord 'A') + 13) `mod` 26)) $ rot13char c)
prop_rot13charNonAlpha c = (not.isAlpha) c ==> (\c' -> c' == c) $ rot13char c

prop_rot13 [] = rot13 "" == ""
prop_rot13 (c:cs) = (\(c':cs') -> c' == rot13char c && cs' == rot13 cs) $ rot13 (c:cs)

checkRot13 = do
    quickCheck prop_rot13charLower
    quickCheck prop_rot13charUpper
    quickCheck prop_rot13charNonAlpha
    quickCheck prop_rot13


-- Exercise 7

-- [JAKOB Insert text]
