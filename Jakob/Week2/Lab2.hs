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

-- 5)

-- 6)

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


-- 7) (60 Min.)

data IbanCountry =
  IbanCountry {
    country      :: String
    , ibanLength :: String
    , code       :: String
    , format     :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

ibanJson :: IO B.ByteString
ibanJson = B.readFile "iban.json"

emptyIbanCountry :: IbanCountry
emptyIbanCountry = IbanCountry "" "" "" ""

ibanCountries :: IO ([IbanCountry])
ibanCountries = do
  json <- ibanJson
  case decode json :: Maybe [IbanCountry] of
    Nothing     -> return [emptyIbanCountry]
    Just result -> return result

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

iban :: String -> IO (Bool)
iban (cc1:cc2:cd1:cd2:xs) = do
  let iban = (cc1:cc2:cd1:cd2:xs)
  let countryCode = [cc1] ++ [cc2]
  validCountryCodes <- countryCodes
  let validCountry = elem countryCode validCountryCodes
  expectedLength <- countryLength countryCode
  let validLength = expectedLength == length iban
  let validCheckDigit = validateIbanCheckDigit iban
  return (validCountry && validLength && validCheckDigit)

{-
  Here goes the test report.
-}
