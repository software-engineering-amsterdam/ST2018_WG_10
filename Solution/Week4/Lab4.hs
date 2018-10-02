{-# LANGUAGE UnicodeSyntax #-}

module Lab4 where

import           Data.List
import           SetOrd
import           System.Random
import           Test.QuickCheck


-- Exercises

-- 1)

{-
  1) How does '(a,b) = {{a},{a,b}}' hold?
  2) What is the practical reasoning behind computing the n-th powerset of
     the empty set?
-}


-- 2)

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


-- 3)

(∩) :: Ord a => Set a -> Set a -> Set a
(Set els) ∩ b = Set $ filter (`inSet` b) els

(∪) :: Ord a => Set a -> Set a -> Set a
(Set as) ∪ (Set bs) = list2set (as ++ bs)

(\\\) :: Ord a => Set a -> Set a -> Set a
(Set els) \\\ b = Set $ filter (not . (`inSet` b)) els


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

  In all three tests, our custom implementation of the set generator as well as
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

{-
  We had no further questions left.
-}


-- 5)

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos []         = []
symClos ((x,y):xs) = (nub $ sort $ (y,x):(x,y):(symClos xs))


-- 6)

-- Some more useful aliases
(⊆), (⊇) :: Ord a => [a] -> [a] -> Bool
a ⊆ b = all (∈ b) a
b ⊇ a = all (∈ b) a

(∈) :: Ord a => a -> [a] -> Bool
a ∈ as = a `elem` as

infixr 5 @@

(@@) :: (Ord a, Eq a) => Rel a -> Rel a -> Rel a
r @@ s = [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: (Ord a, Eq a) => Rel a -> Rel a
trClos [] = []
trClos s@(x:xs) = let s' = (union' (s @@ s) s) in if s' == s then s else trClos s'
    where
        union' xs ys = nub $ sort $ xs ++ ys


-- 7)

{-
  symClos:

  Test properties:
  - Each element in the list is unique
  - Each mirrored relation is also an element of the list
  - There are no extra elements apart from the symmetric ones (smallest relation)
-}

prop_SymClosIsUnique :: Rel Int -> Bool
prop_SymClosIsUnique rel = (nub . symClos $ rel) == symClos rel

-- Takes the same relation as input twice to circumvent the fact that the
-- full relation would otherwise get lost due to the tail recursion
prop_SymClosMirroredElemsHelper :: Rel Int -> Rel Int -> Bool
prop_SymClosMirroredElemsHelper _ []           = True
prop_SymClosMirroredElemsHelper rel ((x,y):xs) = (elem (y,x) rel) && prop_SymClosMirroredElemsHelper rel xs

prop_SymClosMirroredElems :: Rel Int -> Bool
prop_SymClosMirroredElems rel = let symmetricClosure = symClos rel in prop_SymClosMirroredElemsHelper symmetricClosure symmetricClosure

prop_SymClosSmallestRelHelper :: Rel Int -> Rel Int -> Bool
prop_SymClosSmallestRelHelper _ []                          = True
prop_SymClosSmallestRelHelper initRel symClosRel@((x,y):xs) = ((elem (x,y) initRel) || (elem (y,x) initRel)) && prop_SymClosSmallestRelHelper initRel xs

prop_SymClosSmallestRel :: Rel Int -> Bool
prop_SymClosSmallestRel rel = let symmetricClosure = symClos rel in prop_SymClosSmallestRelHelper rel symmetricClosure

check_SymClosIsUnique = quickCheck prop_SymClosIsUnique
check_SymClosMirroredElems = quickCheck prop_SymClosMirroredElems
check_SymClosSmallestRel = quickCheck prop_SymClosSmallestRel


{-
  trClos:

  Test properties:
  ∀ x, y, z: xSy, ySz => xSz
  R ⊆ S
  ∀ S' ⊂ S: S' is not a transitive closure of R
-}

isTransitive :: (Ord a, Eq a) => Rel a -> Bool
isTransitive s = all (\x -> x ∈ s) [(x, z) | (x, y) <- s, (y', z) <- s, y == y']

prop_TrClosTransitive r = isTransitive $ trClos r
    where types = (r :: Rel Int)

prop_TrClosSuperset r = r ⊆ trClos r
    where types = (r :: Rel Int)

prop_TrClosSmallest r = isSmallest $ trClos r
        where
            isSmallest s = r ⊆ s && isTransitive s && (not . hasSmaller) s
            hasSmaller []       = False
            hasSmaller [x]      = isSmallest []
            hasSmaller (x:y:xs) = isSmallest (x:xs) && isSmallest (y:xs)
            types = (r :: Rel Int)

check_TrClos = do
    putStrLn "Exercise 7:"
    putStrLn "Is Transitive:"
    quickCheck prop_TrClosTransitive
    putStrLn "S Contains R:"
    quickCheck prop_TrClosSuperset
    putStrLn "S is minimal:"
    quickCheck prop_TrClosSmallest

{-
  Test report:

  QuickCheck was well suited to test the posed properties.
  The only peculiarity was that in some cases, helper functions were
  needed because e.g. the original relations were still needed to test
  the property recursively – this would not have been possible with just
  using QuickCheck one them since it would have passed two different relations
  into the `prop_*` functions.
-}


-- 8)

prop_transsym ls = trClos (symClos ls) == symClos (trClos ls)
                   where
                     types = (ls :: [(Int,Int)])

lis = [(1,2),(2,3),(3,4)] -- A counterexample
prop_transsym_manual = trClos (symClos lis) == symClos (trClos lis)

exercise8 :: IO()
exercise8 = do
  putStrLn "Test TransSym"
  quickCheck prop_transsym
  putStrLn "Should be 'Falsifiable' -> Counterexample"

-- Not the same, if we use the example relations used in previous exercises
-- we see that the transitive closure of the symmetric closure is different
-- than the reverse.
