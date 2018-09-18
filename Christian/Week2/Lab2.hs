module Lab2 where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import Test.QuickCheck
import Debug.Trace
import Control.Monad 

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

testqwer = 1

-- Excercise 1 - (1:30 Hr.)

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




-- Excercise 2 (1 hr. )


data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


data Triangle a = Triangle a a a deriving (Eq, Ord, Show)

toTriangle :: (Integral a) => a -> a -> a -> Triangle a
toTriangle a b c = Triangle a' b' c'
    where
        [a', b', c'] = sort [a, b, c]

-- In order to more easily test the triangle equation (a + b >= c), the toTriangle function is used that
-- makes sure that c is the long side of the triangle. 
-- 
-- The input is integral, so the cases of Icosceles and Rectangular are mutually exclusive. This
-- is convenient: no strange edge cases need to be accounted for. 
-- Any negative sides do not fulfill the triangle inequality, and are caught in the NoTriangle case.
triangleType :: Integer -> Integer -> Integer -> Shape
triangleType a b c = triangleType' $ toTriangle a b c
    where
        triangleType' (Triangle a b c) | a + b < c         = NoTriangle
                                       | a == b && b == c  = Equilateral
                                       | a == b || b == c  = Isosceles
                                       | a^2 + b^2 == c^2  = Rectangular
                                       | otherwise         = Other

-- The precondition is { }, since all values in the domain (ℤ×ℤ×ℤ) should produce one of the outputs.
-- Negative values do not fulfill the triangle equation, so no special case is necessary.
-- The postcondition is {
--   \x -> x == { 
--      NoTriangle      if a + b < c
--      EquiLateral     if a == b && b == c
--      Icosceles       if a == b || b == c
--      Rectangular     if a^2 + b^2 == c^2
--      Other           otherwise
--   }
-- 
-- where c is the long side of the triangle.
-- 
-- This is easily seen, and testing for this would only replicate the above code.
-- The assumption that c is the long side means that the helper function triangleType' has 
-- the precondition: {c >= a, c >= b}.
-- We can test if this precondition is enforced by toTriangle as follows:

checkLongSide = quickCheck $ longSideProp
    where
        longSideProp :: Integer -> Integer -> Integer -> Bool
        longSideProp a b c = (\(Triangle a' b' c') -> c' >= a' && c' >= b') $ (toTriangle a b c)

-- >>> checkLongSide
-- > OK, passed 100 tests



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
isSorted _ [] = True
isSorted _ [_] = True
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

isDerangement :: (Integral a) => [a] -> [a] -> Bool
isDerangement xs ys = sort xs == sort ys && isDerangement' xs ys
    where
        isDerangement' [] [] = True
        isDerangement' (x:xs) (y:ys) = x /= y && isDerangement' xs ys


deran :: (Integral a) => [a] -> [[a]]
deran [] = []
deran xs = filter (isDerangement xs) (permutations xs)



-- Exercise 6


-- {\(c:cs) -> isLower c } \(c:cs) -> rot13 c:cs  {\(c':cs') -> c' = 'a' + ((c - 'a' + 13) `mod` 26) }
-- {\(c:cs) -> isUpper c } \(c:cs) -> rot13 c:cs  {\(c':cs') -> c' = 'A' + ((c - 'A' + 13) `mod` 26) }
-- {\(c:cs) -> isNonAlpha c } \(c:cs) -> rot13 c:cs  {\(c':cs') -> c' = c }

rot13 :: String -> String
rot13 [] = []
rot13 (c:cs) = (rotc c):(rot13 cs)
    where
        rotc :: Char -> Char
        rotc c | isLower c = chr $ ord 'a' + ((ord c - ord 'a' + 13) `mod` 26)
        rotc c | isUpper c = chr $ ord 'A' + ((ord c - ord 'A' + 13) `mod` 26)
        rotc c = c



countrycodes = "AF","AX","AL","DZ","AS","AD","AO","AI","AQ","AG","AR","AM","AW","AU","AT","AZ","BS","BH","BD","BB","BY","BE","BZ","BJ","BM","BT","BO","BQ","BA","BW","BV","BR","IO","BN","BG","BF","BI","KH","CM","CA","CV","KY","CF","TD","CL","CN","CX","CC","CO","KM","CG","CD","CK","CR","CI","HR","CU","CW","CY","CZ","DK","DJ","DM","DO","EC","EG","SV","GQ","ER","EE","ET","FK","FO","FJ","FI","FR","GF","PF","TF","GA","GM","GE","DE","GH","GI","GR","GL","GD","GP","GU","GT","GG","GN","GW","GY","HT","HM","VA","HN","HK","HU","IS","IN","ID","IR","IQ","IE","IM","IL","IT","JM","JP","JE","JO","KZ","KE","KI","KP","KR","KW","KG","LA","LV","LB","LS","LR","LY","LI","LT","LU","MO","MK","MG","MW","MY","MV","ML","MT","MH","MQ","MR","MU","YT","MX","FM","MD","MC","MN","ME","MS","MA","MZ","MM","NA","NR","NP","NL","NC","NZ","NI","NE","NG","NU","NF","MP","NO","OM","PK","PW","PS","PA","PG","PY","PE","PH","PN","PL","PT","PR","QA","RE","RO","RU","RW","BL","SH","KN","LC","MF","PM","VC","WS","SM","ST","SA","SN","RS","SC","SL","SG","SX","SK","SI","SB","SO","ZA","GS","SS","ES","LK","SD","SR","SJ","SZ","SE","CH","SY","TW","TJ","TZ","TH","TL","TG","TK","TO","TT","TN","TR","TM","TC","TV","UG","UA","AE","GB","US","UM","UY","UZ","VU","VE","VN","VG","VI","WF","EH","YE","ZM","ZW"


