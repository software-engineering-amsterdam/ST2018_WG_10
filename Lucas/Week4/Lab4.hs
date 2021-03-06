module Lab4 where

import Data.List
import Data.Tuple
import Data.Traversable
import System.Random
import System.IO.Unsafe
import Control.Monad.Random
import Test.QuickCheck
import SetOrd



--Exercise 2 | Time spent (2:30) <- for only one line hahaha, i suck at quickcheck :)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary

randSet :: Int -> Set Int
randSet n = list2set (unsafePerformIO (randomList n))

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,40)
  rs <- randomList (n-1)
  return (r:rs) 


--Exercise 3 | Time Spent (1:00)

interSet :: (Ord a) => Set a -> Set a -> Set a -> Set a
interSet i (Set []) s2 = i
interSet i s1 (Set []) = emptySet
interSet i (Set (x:xs)) s2 = if inSet x s2 then interSet (insertSet x i) (Set xs) s2 else interSet i (Set xs) s2


diffSet :: (Ord a) => Set a -> Set a -> Set a -> Set a
diffSet i (Set []) s2 = i
diffSet i (Set (x:xs)) s2 = if inSet x s2 then diffSet i (Set xs) s2 else diffSet (insertSet x i) (Set xs) s2
--Union of a set already exists in SetOrd.hs


--If the intersection between the same set is that set, then the intersection works
manualTestInters :: Int -> Bool
manualTestInters 0 = True
manualTestInters n = do
                let rset = randSet 20
                if interSet emptySet rset rset == rset
                    then manualTestInters (n-1)
                    else False

--If the difference between the same set is an empty set thend the difference works
manualTestDiff :: Int -> Bool
manualTestDiff 0 = True
manualTestDiff n = do
                let rset = randSet 20
                if diffSet emptySet rset rset == emptySet
                    then manualTestDiff (n-1)
                    else False


--If the union of 2 equal sets is equal to the original set then the union works
manualTestUn :: Int -> Bool
manualTestUn 0 = True
manualTestUn n = do
                let rset = randSet 20
                if unionSet rset rset == rset
                    then manualTestUn (n-1)
                    else False


prop_inters ls = interSet emptySet ls ls == ls
           where types = (ls :: (Set Int))

prop_diff ls = diffSet emptySet ls ls == emptySet
           where types = (ls :: (Set Int))

prop_union ls = unionSet ls ls == ls
           where types = (ls :: (Set Int))

ex3 :: IO()
ex3 = do
    putStrLn "QUICKCHECK"
    putStrLn "=============================="
    putStrLn "Test intersect"
    putStr " >  "
    quickCheck prop_inters
    putStrLn ""
    putStrLn "Test diff"
    putStr " >  "
    quickCheck prop_diff
    putStrLn ""
    putStrLn "Test union"
    putStr " >  "
    quickCheck prop_union
    putStrLn ""
    putStrLn "=============================="
    putStrLn "MANUAL CHECK"
    putStrLn "Test intersect"
    putStr " >  "
    putStrLn (if manualTestInters 100 then "True" else "False")
    putStrLn ""
    putStrLn "Test diff"
    putStr " >  "
    putStrLn (if manualTestDiff 100 then "True" else "False")
    putStrLn ""
    putStrLn "Test union"
    putStr " >  "
    putStrLn (if manualTestUn 100 then "True" else "False")


--Exercise 5 | Time Spent (15min)
type Rel a = [(a,a)]

invert = map swap

symClos :: Ord a => Rel a -> Rel a
symClos t = sort (t ++ (invert t))

--Exercise 6 | Time Spent (1:00)

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--The integer parameter is used as depth for the closure, put in the length of the relation if you want the complete closure
trClos :: Ord a => Int -> Rel a -> Rel a
trClos 0 a = sort a
trClos n a = trClos (n-1) (nub (a ++ (a @@ a)))

--Exercise 7 | Time Spent (-)



--Exercise 8 | Time Spent (00:30)
--instance (Ord a, Arbitrary a) => Arbitrary (Rel a) where
--    arbitrary = arbitrary :: [(a,a)]

--Cant be tuples like [(0,0)]
prop_transsym ls = trClos (length ls) (symClos ls) == symClos (trClos (length ls) ls)
           where types = (ls :: [(Int,Int)])

lis = [(1,2),(2,3),(3,4)] --A counterexample
prop_transsym_manual = trClos (length lis) (symClos lis) == symClos (trClos (length lis) lis)

ex8 :: IO()
ex8 = do
    putStrLn "Test TransSym"
    putStr " >  "
    quickCheck prop_transsym
    putStrLn ""

--Not the same, if we use the example relations used in previous exercises we see that the transitive closure of the symmetric closure is different than the reverse
