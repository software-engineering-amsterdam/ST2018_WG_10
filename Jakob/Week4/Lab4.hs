{-# LANGUAGE UnicodeSyntax #-}

module Lab4 where

import           Data.List
import           SetOrd
import           System.Random
import           Test.QuickCheck


-- Exercises

-- 1)




-- 2) (15 Min.)

-- From scratch
genRandIntBetween1And10 :: IO Int
genRandIntBetween1And10 = randomRIO (1 :: Int, 10 :: Int)

setGenScratch :: IO (Set Int)
setGenScratch = do
  length <- genRandIntBetween1And10
  list <- sequence (replicate length genRandIntBetween1And10)
  return (list2set list)

-- With QuickCheck
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = fmap list2set arbitrary


-- 3) (120 Min.)

-- Intersection
listIntersect :: Ord a => Set a -> Set a -> [a]
listIntersect (Set []) setB     =
  []
listIntersect (Set (a:as)) setB =
  if inSet a setB == True then
    [a] ++ (listIntersect (Set as) setB)
  else
    [] ++ (listIntersect (Set as) setB)

(∩) :: Ord a => Set a -> Set a -> Set a
a ∩ b = list2set (listIntersect a b)

-- Union
(∪) :: Ord a => Set a -> Set a -> Set a
(Set a) ∪ (Set b) = list2set (a ++ b)

-- Difference
listDiff :: Ord a => Set a -> Set a -> [a]
listDiff (Set []) setB     =
  []
listDiff (Set (a:as)) setB =
  if inSet a setB == False then
    [a] ++ (listDiff (Set as) setB)
  else
    [] ++ (listDiff (Set as) setB)

(\\\) :: Ord a => Set a -> Set a -> Set a
a \\\ b = list2set (listDiff a b)


{-
  Intersection:

  Property: Every element of the intersection of set A and set B should also be
  an element of both of those initial sets.
-}
intersectCompsContain :: Eq a => [a] -> Set a -> Set a -> Bool
intersectCompsContain [] _ _                           = True
intersectCompsContain (i:is) setA@(Set a) setB@(Set b) = (elem i a) && (elem i b) && intersectCompsContain is setA setB

custom_prop_Intersect :: (Ord a) => IO (Set a) -> IO (Set a) -> IO Bool
custom_prop_Intersect ioA ioB = do
  a <- ioA
  b <- ioB
  let (Set cs) = (a ∩ b)
  return (intersectCompsContain cs a b)

custom_intersectTest :: IO Bool
custom_intersectTest = custom_prop_Intersect setGenScratch setGenScratch

custom_intersectCheck :: IO ()
custom_intersectCheck = do
  xs <- (sequence (replicate 100 custom_intersectTest))
  putStrLn ("+++ Passed " ++ (show . length . (uncurry filter) $ ((== True), xs)) ++ " tests.")

prop_Intersect :: Set Int -> Set Int -> Bool
prop_Intersect a b = let (Set cs) = (a ∩ b) in (intersectCompsContain cs a b)

intersectCheck :: IO ()
intersectCheck = verboseCheck prop_Intersect


{-
  Union:

  Property: All elements from set A and set B should also be an element of
  the union of both sets.
-}
compsContainedInUnion :: Eq a => Set a -> Set a -> Set a -> Bool
compsContainedInUnion (Set []) (Set []) (Set (us)) = True
compsContainedInUnion (Set (a:as)) (Set []) (Set (us)) = (elem a us) && True
compsContainedInUnion (Set []) (Set (b:bs)) (Set (us)) = True && (elem b us)
compsContainedInUnion (Set (a:as)) (Set (b:bs)) setUnion@(Set (us)) = (elem a us) && (elem b us) && compsContainedInUnion (Set as) (Set bs) setUnion

custom_prop_Union :: (Ord a) => IO (Set a) -> IO (Set a) -> IO Bool
custom_prop_Union ioA ioB = do
  a <- ioA
  b <- ioB
  let u = (a ∪ b)
  return (compsContainedInUnion a b u)

custom_UnionTest :: IO Bool
custom_UnionTest = custom_prop_Union setGenScratch setGenScratch

custom_UnionCheck :: IO ()
custom_UnionCheck = do
  xs <- (sequence (replicate 100 custom_UnionTest))
  putStrLn ("+++ Passed " ++ (show . length . (uncurry filter) $ ((== True), xs)) ++ " tests.")

prop_Union :: Set Int -> Set Int -> Bool
prop_Union a b = let u = (a ∪ b) in (compsContainedInUnion a b u)

unionCheck :: IO ()
unionCheck = verboseCheck prop_Union


{-
  Difference:

  Property: No element from the difference of set A and set B should also be an
  element of set B.
-}
compBNotContainedInDiff :: Eq a => Set a -> Set a -> Bool
compBNotContainedInDiff _ (Set [])                = True
compBNotContainedInDiff setB@(Set b) (Set (d:ds)) = (not (elem d b)) && compBNotContainedInDiff setB (Set ds)

custom_prop_Diff :: (Ord a) => IO (Set a) -> IO (Set a) -> IO Bool
custom_prop_Diff ioA ioB = do
  a <- ioA
  b <- ioB
  let d = (a \\\ b)
  return (compBNotContainedInDiff b d)

custom_DiffTest :: IO Bool
custom_DiffTest = custom_prop_Diff setGenScratch setGenScratch

custom_DiffCheck :: IO ()
custom_DiffCheck = do
  xs <- (sequence (replicate 100 custom_DiffTest))
  putStrLn ("+++ Passed " ++ (show . length . (uncurry filter) $ ((== True), xs)) ++ " tests.")

prop_Diff :: Set Int -> Set Int -> Bool
prop_Diff a b = let d = (a \\\ b) in (compBNotContainedInDiff b d)

diffCheck :: IO ()
diffCheck = verboseCheck prop_Diff


{-
  Test report:

  In all three tests, my custom implementation of the set generator as well as
  the QuickCheck version were used to test the implemented operators.
  For each operator a helper function was used that checks the test property of
  each operator specified above. Thus, it is always about checking whether
  elements are part of other sets passed into the helper functions.
  Those helper functions were used for both the custom generator as well as
  the QuickCheck version. For the custom one, two generated sets were used
  and tested, this function was replicated 100 times and the amount of
  successful tests gets output.
  For QuickCheck, a normal `prop_*` function was created to easily run it.
  However, it is important to note that those functions are in need of a type
  signature under every circumstance. For this reason, `Set Int` was chosen
  as the type of the two input arguments. Otherwise, QuickCheck runs the tests
  with empty tuples instead of `Int`. To verify this, `verboseCheck` was used.
-}


-- 4)




-- 5) (10 Min.)

type Rel a = [(a,a)]

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

symClos :: Ord a => Rel a -> Rel a
symClos []         = []
symClos ((x,y):xs) = removeDuplicates ((x,y) : (y,x) : symClos xs)


-- 6)




-- 7)




-- 8)
