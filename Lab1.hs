module Lab1 where
import Data.List
import Data.Char (digitToInt)
import Test.QuickCheck
import Data.List (sort, subsequences)

toDigits :: Integer -> [Integer]
toDigits = map (fromIntegral . digitToInt) . show

doubleFromRight :: [Integer] -> [Integer]
doubleFromRight xs = fst $ foldr (\x (acc, bool) ->
                                  ((if bool then 2 * x else x) : acc,
                                   not bool)) ([], False) xs

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

boomBangs xs = [ x | x <- xs, x <= 20, x >= 10, x /= 13, x /= 15, x /= 19]

dif2Cntr :: [Integer] -> [(Integer, Integer)]
dif2Cntr (a:b:rs) = if a<50 then (a,b) : dif2Cntr(a+1:rs)
 else dif2Cntr(a+1:rs)

primePairs = dif2Cntr primes 

--1
sumNatSq :: [Int] -> Bool
sumNatSq o = sum(map (^2) o)*6 == (length o * (length o+1) * (length o*2+1))


--2


--3


--4
reversePrimes :: [Integer]
reversePrimes = [x | x <- take 10000 primes, prime (reversal x)]

--5


getMinSum101Prime :: Integer
getMinSum101Prime = head[(sum (take ((x+101)-x) (drop x primes)))|x <- [0..10000], prime (sum (take ((x+101)-x) (drop x primes)))]

--



--6

consecutivePrimeCheck :: [Integer]
consecutivePrimeCheck = [product(take p primes) | p <- [0..6],not (prime(product(take p primes)+1))]


--7
minus9 q = q-9
luhn :: Integer -> Bool
luhn n = (0 == rem (sum (map (minus9) ( filter (>10) (doubleFromRight (toDigits n))))) 10)


--8
--data Boy = Matthew | Peter | Jack | Arnold | Carl
--	deriving (Eq,Show)

-- boys = [Matthew, Peter, Jack, Arnold, Carl]

--accuses :: Boy -> Boy -> Bool
--accuses Matthew Matthew = False
--accuses Matthew Peter = True 
--accuses Matthew Jack = True
--accuses Matthew Arnold = True
--accuses Matthew Carl = False

--accuses Peter Peter = False
--accuses Peter Matthew = True 
--accuses Peter Jack = True
--accuses Peter Arnold = False
--accuses Peter Carl = False

--accuses Jack x = not (Matthew x and Peter x)

--accuses Arnold x = ((not (Matthew x)) and (Peter x)) or ((Matthew x) and (not (Peter x))) 

--accuses Carl x = not (Arnold x)


--accusers :: Boy -> [Boy]
--guilty, honest :: [Boy]
