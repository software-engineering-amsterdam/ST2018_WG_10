{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lab2 where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Char
import           Data.List
import           Data.Maybe
import           GHC.Generics
import           System.Random
import           Test.QuickCheck

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
           | Isosceles  | Rectangular | Other deriving (Eq, Show)

lowercaseAlphabet = ['a'..'z']

uppercaseAlphabet = ['A'..'Z']


-- Exercises

-- 1) (60 Min.)

mean :: [Float] -> Float
mean floats = (sum floats) / (fromIntegral (length floats))

variance :: [Float] -> Float
variance floats = mean squaredDiffs
                  where
                    average = mean floats
                    diffs = map ((-)average) floats
                    squaredDiffs = map (^2) diffs

standardDev :: [Float] -> Float
standardDev [] = 0
standardDev xs = sqrt . variance $ xs

fromIntLength :: (Num c, Foldable t) => t a -> c
fromIntLength = fromIntegral . length

quartileLenghts :: (Ord a, Fractional a) => [a] -> [Float]
quartileLenghts floats = [fromIntLength first, fromIntLength second, fromIntLength third, fromIntLength fourth]
                         where
                           first  = takeWhile (<0.25) (dropWhile (<0.00) floats)
                           second = takeWhile (<0.50) (dropWhile (<0.25) floats)
                           third  = takeWhile (<0.75) (dropWhile (<0.50) floats)
                           fourth = takeWhile (<1.00) (dropWhile (<0.75) floats)

probsTest :: Int -> IO Float
probsTest n = do
  floats <- probs n
  let sortedFloats = sort floats
  let quartiles = quartileLenghts sortedFloats
  let stdDev = standardDev quartiles
  return stdDev

probsCheck :: IO ()
probsCheck = do
  stdDev <- probsTest 10000
  if stdDev <= 50.0 then
    putStrLn $ "The standard deviation is acceptable (" ++ show stdDev ++ ")."
  else
    putStrLn $ "The standard deviation is too high (" ++ show stdDev ++ ")."

{-
  The `probs :: Int -> IO [Float]` function by Red Curry seems to yield very
  differing results on every run. The custom function `probsTest :: IO Float`
  retrieves n floats from `probs` and calculates the standard deviation of the
  quantiles of (0..1).
  The custom function `probsCheck :: IO ()` runs `probsTest` with 10000 as its
  input. It was determined, that a standard deviation under 50 is acceptable
  while one over 50 is too high.
  Consequently, it can be observed that the result differs vastly on each run.
  This leads to believe that the function `probs` is not properly working.
-}


-- 2) (20 Min.)

pythagorean :: (Integer, Integer, Integer) -> Bool
pythagorean (a,b,c) = a^2 + b^2 == c^2

triangleImpossible :: (Integer, Integer, Integer) -> Bool
triangleImpossible (a,b,c) = (a + b) < c

tripleIsEqual :: (Integer, Integer, Integer) -> Bool
tripleIsEqual (x,y,z) = (x == y) && (y == z) && (x == z)

twoInTripleAreEqual :: (Integer, Integer, Integer) -> Bool
twoInTripleAreEqual (x,y,z) = x == y || y == z || x == z

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | triangleImpossible (a,b,c)  = NoTriangle
               | tripleIsEqual (a,b,c)       = Equilateral
               | pythagorean (a,b,c)         = Rectangular
               | twoInTripleAreEqual (a,b,c) = Isosceles
               | otherwise                   = Other

{-
  Here goes the test report.
-}

-- 3)

-- 4)

-- 5) (45 Min.)

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
derangementTest n = (n >= 2 && n <= 7) ==> length (deran [0..(n-1)]) == subfact n

derangementQuickCheck = quickCheck derangementTest

{-
  To test if `isDerangement` is actually working, I made use of the fact
  that the length of the derangements of a list `[0..(n-1)]` is the subfactorial
  of n. Actually, any list of length n works in the same fashion.
  Consequently, I wrote a function `subfact` that calculates the subfactorial.
  Finally, I wrote a function that tests if the stated property holds.
  The only problem is that calculating the derangements and the subfactorial
  for integers larger than 10 takes a really long time or might not finish at all.
  Therefore, I limited the numbers QuickCheck tests to numbers not larger than 8.
  This unfortunately does not generate enough tests for QuickCheck and the test
  'gives up'. However, the general idea works and all of the ran tests do pass.
-}


-- 6) (45 Min.)

{-
  ROT13 is a simple cipher with no real cryptographic security.
  The idea is that every letter of a given string are shifted
  by 13 letters based on the latin alphabet.
  This makes the output string unreadable for humans.
  Owing to the fact, that the latin alphabet bears 26 letters,
  applying ROT13 to a previously ciphered string with the same
  algorithm gives back the original string since on every run,
  each letter gets essentially inverted on the "scale" of the alphabet.
-}

-- Only works on lowercase strings
rot13 :: String -> String
rot13 [x]    = [lowercaseAlphabet !! (mod (ord x - ord 'a' + 13) 26)]
rot13 (x:xs) = rot13 [x] ++ rot13 xs

{-
  Here goes the test report.
-}


-- 7) (120 Min.)

-- Only works with `Data.Aeson` imported
-- data IbanCountry =
--   IbanCountry {
--     country      :: String
--     , ibanLength :: String
--     , code       :: String
--     , format     :: String
--   } deriving (Show, Generic, ToJSON, FromJSON)

data IbanCountry =
  IbanCountry {
    country      :: String
    , ibanLength :: String
    , code       :: String
    , format     :: String
  } deriving (Show)

ibanJson :: IO B.ByteString
ibanJson = B.readFile "iban.json"

emptyIbanCountry :: IbanCountry
emptyIbanCountry = IbanCountry "" "" "" ""

-- Only works with `Data.Aeson` imported
-- ibanCountries :: IO ([IbanCountry])
-- ibanCountries = do
--   json <- ibanJson
--   case decode json :: Maybe [IbanCountry] of
--     Nothing     -> return [emptyIbanCountry]
--     Just result -> return result

-- Substitution for missing `Data.Aeson`
ibanCountries :: IO [IbanCountry]
ibanCountries = pure ([IbanCountry {country = "Albania", ibanLength = "28", code = "AL", format = "AL2!n8!n16!c"},IbanCountry {country = "Andorra", ibanLength = "24", code = "AD", format = "AD2!n4!n4!n12!c"},IbanCountry {country = "Austria", ibanLength = "20", code = "AT", format = "AT2!n5!n11!n"},IbanCountry {country = "Azerbaijan, Republic of", ibanLength = "28", code = "AZ", format = "AZ2!n4!a20!c"},IbanCountry {country = "Bahrain", ibanLength = "22", code = "BH", format = "BH2!n4!a14!c"},IbanCountry {country = "Belgium", ibanLength = "16", code = "BE", format = "BE2!n3!n7!n2!n"},IbanCountry {country = "Bosnia and Herzegovina", ibanLength = "20", code = "BA", format = "BA2!n3!n3!n8!n2!n"},IbanCountry {country = "Brazil", ibanLength = "29", code = "BR", format = "BR2!n8!n5!n10!n1!a1!c"},IbanCountry {country = "Bulgaria", ibanLength = "22", code = "BG", format = "BG2!n4!a4!n2!n8!c"},IbanCountry {country = "Costa Rica", ibanLength = "21", code = "CR", format = "CR2!n3!n14!n"},IbanCountry {country = "Croatia", ibanLength = "21", code = "HR", format = "HR2!n7!n10!n"},IbanCountry {country = "Cyprus", ibanLength = "28", code = "CY", format = "CY2!n3!n5!n16!c"},IbanCountry {country = "Czech Republic", ibanLength = "24", code = "CZ", format = "CZ2!n4!n6!n10!n"},IbanCountry {country = "Denmark", ibanLength = "18", code = "DK", format = "DK2!n4!n9!n1!n"},IbanCountry {country = "Dominican Republic", ibanLength = "28", code = "DO", format = "DO2!n4!c20!n"},IbanCountry {country = "Estonia", ibanLength = "20", code = "EE", format = "EE2!n2!n2!n11!n1!n"},IbanCountry {country = "Faroe Islands", ibanLength = "18", code = "FO", format = "FO2!n4!n9!n1!n"},IbanCountry {country = "Finland", ibanLength = "18", code = "FI", format = "FI2!n6!n7!n1!n"},IbanCountry {country = "France", ibanLength = "27", code = "FR", format = "FR2!n5!n5!n11!c2!n"},IbanCountry {country = "Georgia", ibanLength = "22", code = "GE", format = "GE2!n2!a16!n"},IbanCountry {country = "Germany", ibanLength = "22", code = "DE", format = "DE2!n8!n10!n"},IbanCountry {country = "Gibraltar", ibanLength = "23", code = "GI", format = "GI2!n4!a15!c"},IbanCountry {country = "Greece", ibanLength = "27", code = "GR", format = "GR2!n3!n4!n16!c"},IbanCountry {country = "Greenland", ibanLength = "18", code = "GL", format = "GL2!n4!n9!n1!n"},IbanCountry {country = "Guatemala", ibanLength = "28", code = "GT", format = "GT2!n4!c20!c"},IbanCountry {country = "Hungary", ibanLength = "28", code = "HU", format = "HU2!n3!n4!n1!n15!n1!n"},IbanCountry {country = "Iceland", ibanLength = "26", code = "IS", format = "IS2!n4!n2!n6!n10!n"},IbanCountry {country = "Ireland", ibanLength = "22", code = "IE", format = "IE2!n4!a6!n8!n"},IbanCountry {country = "Israel", ibanLength = "23", code = "IL", format = "IL2!n3!n3!n13!n"},IbanCountry {country = "Italy", ibanLength = "27", code = "IT", format = "IT2!n1!a5!n5!n12!c"},IbanCountry {country = "Jordan", ibanLength = "30", code = "JO", format = "JO2!n4!a4!n18!c"},IbanCountry {country = "Kazakhstan", ibanLength = "20", code = "KZ", format = "KZ2!n3!n13!c"},IbanCountry {country = "Kuwait", ibanLength = "30", code = "KW", format = "KW2!n4!a22!c"},IbanCountry {country = "Latvia", ibanLength = "21", code = "LV", format = "LV2!n4!a13!c"},IbanCountry {country = "Lebanon", ibanLength = "28", code = "LB", format = "LB2!n4!n20!c"},IbanCountry {country = "Liechtenstein (Principality of)", ibanLength = "21", code = "LI", format = "LI2!n5!n12!c"},IbanCountry {country = "Lithuania", ibanLength = "20", code = "LT", format = "LT2!n5!n11!n"},IbanCountry {country = "Luxembourg", ibanLength = "20", code = "LU", format = "LU2!n3!n13!c"},IbanCountry {country = "Macedonia", ibanLength = "19", code = "MK", format = "MK2!n3!n10!c2!n"},IbanCountry {country = "Malta", ibanLength = "31", code = "MT", format = "MT2!n4!a5!n18!c"},IbanCountry {country = "Mauritania", ibanLength = "27", code = "MR", format = "MR2!n5!n5!n11!n2!n"},IbanCountry {country = "Mauritius", ibanLength = "30", code = "MU", format = "MU2!n4!a2!n2!n12!n3!n3!a"},IbanCountry {country = "Moldova", ibanLength = "24", code = "MD", format = "MD2!n2!c18!c"},IbanCountry {country = "Monaco", ibanLength = "27", code = "MC", format = "MC2!n5!n5!n11!c2!n"},IbanCountry {country = "Montenegro", ibanLength = "22", code = "ME", format = "ME2!n3!n13!n2!n"},IbanCountry {country = "Netherlands", ibanLength = "18", code = "NL", format = "NL2!n4!a10!n"},IbanCountry {country = "Norway", ibanLength = "15", code = "NO", format = "NO2!n4!n6!n1!n"},IbanCountry {country = "Pakistan", ibanLength = "24", code = "PK", format = "PK2!n4!a16!c"},IbanCountry {country = "Poland", ibanLength = "28", code = "PL", format = "PL2!n8!n16!n"},IbanCountry {country = "Palestinian Territory, Occupied", ibanLength = "29", code = "PS", format = "PS2!n4!a21!c"},IbanCountry {country = "Portugal", ibanLength = "25", code = "PT", format = "PT2!n4!n4!n11!n2!n"},IbanCountry {country = "Qatar", ibanLength = "29", code = "QA", format = "QA2!n4!a21!c"},IbanCountry {country = "Republic of Kosovo", ibanLength = "20", code = "XK", format = "XK2!n4!n10!n2!n"},IbanCountry {country = "Romania", ibanLength = "24", code = "RO", format = "RO2!n4!a16!c"},IbanCountry {country = "Saint Lucia", ibanLength = "32", code = "LC", format = "LC2!n4!a24!c"},IbanCountry {country = "San Marino", ibanLength = "27", code = "SM", format = "SM2!n1!a5!n5!n12!c"},IbanCountry {country = "Sao Tome And Principe", ibanLength = "25", code = "ST", format = "ST2!n8!n11!n2!n"},IbanCountry {country = "Saudi Arabia", ibanLength = "24", code = "SA", format = "SA2!n2!n18!c"},IbanCountry {country = "Serbia", ibanLength = "22", code = "RS", format = "RS2!n3!n13!n2!n"},IbanCountry {country = "Seychelles", ibanLength = "31", code = "SC", format = "SC2!n4!a2!n2!n16!n3!a"},IbanCountry {country = "Slovak Republic", ibanLength = "24", code = "SK", format = "SK2!n4!n6!n10!n"},IbanCountry {country = "Slovenia", ibanLength = "19", code = "SI", format = "SI2!n5!n8!n2!n"},IbanCountry {country = "Spain", ibanLength = "24", code = "ES", format = "ES2!n4!n4!n1!n1!n10!n"},IbanCountry {country = "Sweden", ibanLength = "24", code = "SE", format = "SE2!n3!n16!n1!n"},IbanCountry {country = "Switzerland", ibanLength = "21", code = "CH", format = "CH2!n5!n12!c"},IbanCountry {country = "Timor-Leste", ibanLength = "23", code = "TL", format = "TL2!n3!n14!n2!n"},IbanCountry {country = "Tunisia", ibanLength = "24", code = "TN", format = "TN2!n2!n3!n13!n2!n"},IbanCountry {country = "Turkey", ibanLength = "26", code = "TR", format = "TR2!n5!n1!c16!c"},IbanCountry {country = "Ukraine", ibanLength = "29", code = "UA", format = "UA2!n6!n19!c"},IbanCountry {country = "United Arab Emirates", ibanLength = "23", code = "AE", format = "AE2!n3!n16!n"},IbanCountry {country = "United Kingdom", ibanLength = "22", code = "GB", format = "GB2!n4!a6!n8!n"},IbanCountry {country = "Virgin Islands, British", ibanLength = "24", code = "VG", format = "VG2!n4!a16!n"},IbanCountry {country = "TestCountry1", ibanLength = "34", code = "YY", format = "YY2!n4!a8!n18!c"},IbanCountry {country = "TestCountry2", ibanLength = "35", code = "ZZ", format = "ZZ2!n4!a9!n18!c"}]) :: IO ([IbanCountry])

convertIbanCharToDigit :: Char -> String
convertIbanCharToDigit x = show ((fromMaybe (0) (elemIndex x uppercaseAlphabet)) + 10)

rearrangeIban :: String -> String
rearrangeIban (cc1:cc2:cd1:cd2:xs) = xs ++ [cc1] ++ [cc2] ++ [cd1] ++ [cd2]

convertRearrangedIbanToInteger :: String -> Integer
convertRearrangedIbanToInteger xs = read convertedIban :: Integer
                                    where
                                      convertedIban = concat [if not(isDigit x) then convertIbanCharToDigit x else [x] | x <- xs]

validateIbanCheckDigit :: String -> Bool
validateIbanCheckDigit xs = (mod ibanAsInt 97) == 1
                            where
                              rearrangedIban = rearrangeIban xs
                              ibanAsInt      = convertRearrangedIbanToInteger rearrangedIban

extractCountryCodes :: [IbanCountry] -> [String]
extractCountryCodes [x]    = [Lab2.code x]
extractCountryCodes (x:xs) = [Lab2.code x] ++ extractCountryCodes xs

countryCodes :: IO [String]
countryCodes = do
  countries <- ibanCountries
  return (extractCountryCodes countries)

findCountryLength :: [IbanCountry] -> String -> Int
findCountryLength [] _      = (-1)
findCountryLength (x:xs) cc = if Lab2.code x == cc then read (Lab2.ibanLength x) :: Int else findCountryLength xs cc

countryLength :: String -> IO Int
countryLength countryCode = do
  countries <- ibanCountries
  return (findCountryLength countries countryCode)

iban :: String -> IO Bool
iban (cc1:cc2:cd1:cd2:xs) = do
  let iban = (cc1:cc2:cd1:cd2:xs)
  let countryCode = [cc1] ++ [cc2]
  validCountryCodes <- countryCodes
  let validCountry = elem countryCode validCountryCodes
  expectedLength <- countryLength countryCode
  let validLength = expectedLength == length iban
  let validCheckDigit = validateIbanCheckDigit iban
  return (validCountry && validLength && validCheckDigit)

validIbans :: [String]
validIbans = ["AL35202111090000000001234567", "AT483200000012345864", "MT31MALT01100000000000000000123", "SC52BAHL01031234567890123456USD", "NO8330001234567"]

validIbansTest :: IO [Bool]
validIbansTest = sequence (map iban validIbans)

incorrectCountryCode :: IO String
incorrectCountryCode = do
  randomPos1 <- randomRIO (0, ((length uppercaseAlphabet) - 1))
  let firstLetter = uppercaseAlphabet !! randomPos1
  randomPos2 <- randomRIO (0, ((length uppercaseAlphabet) - 1))
  let secondLetter = uppercaseAlphabet !! randomPos2
  let countryCode = firstLetter:secondLetter:[]
  validCountryCodes <- countryCodes
  if not(elem countryCode validCountryCodes) then
    return countryCode
  else
    incorrectCountryCode

smallestNumberForAmountOfDigits :: Int -> Integer
smallestNumberForAmountOfDigits n = read ("1" ++ concat (replicate n (show 0))) :: Integer

largestNumberForAmountOfDigits :: Int -> Integer
largestNumberForAmountOfDigits n = read ("9" ++ concat (replicate n (show 9))) :: Integer

incorrectIbanLength :: IO Integer
incorrectIbanLength = do
  randomTooLongIban <- randomRIO (smallestNumberForAmountOfDigits 35, largestNumberForAmountOfDigits 35)
  return randomTooLongIban

generateInvalidIban :: IO String
generateInvalidIban = do
  countryCode <- incorrectCountryCode
  randomIbanDigits <- incorrectIbanLength
  return (countryCode ++ (show randomIbanDigits))

invalidIbanTest :: IO Bool
invalidIbanTest = do
  invalidIban <- generateInvalidIban
  iban invalidIban

invalidIbanCheck :: IO [Bool]
invalidIbanCheck = sequence (replicate 100 invalidIbanTest)

countryCodeTest = do
  countryCode <- incorrectCountryCode
  validCountryCodes <- countryCodes
  return (elem countryCode validCountryCodes)

countryCodeCheck = do
  l <- sequence (replicate 100000 countryCodeTest)
  return (foldl1 (||) l)

{-
  There are two ways of varifying that the `iban` function is working.
  One can test actual, valid IBANs or generate invalid ones on purpose.

  For testing actual IBANs, I just used a list of valid ones out of the internet.
  For testing invalid ones, I wrote a function that generates a non-existent
  country code and an IBAN that is 37 digits in length – no country's IBANs
  are that long. The function `generateInvalidIban` combines it to a full IBAN.
  Finally, `invalidIbanTest` runs the `iban` function on a randomly generated
  invalid IBAN and always yields false. This is verified by `invalidIbanCheck`
  which runs it 100 times and returns :: IO [False].

  This way, both valid as well as invalid IBANs can be tested and yield the correct results.
-}
