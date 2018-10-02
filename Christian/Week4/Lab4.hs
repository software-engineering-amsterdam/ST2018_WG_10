module Lab4 where

import SetOrd
import System.Random
import Test.QuickCheck
import Debug.Trace
import qualified Lecture4

-- Util --

instance Foldable Set where
    foldr f b (Set as) = foldr f b as

-- Exercise 1. --

{-

I have no further questions.
-}


-- Exercise 2. --
-- =========== --

-- Non-quickcheck implementation to generate a 
-- random set of max length N:
randomSet :: Int -> IO (Set Int)
randomSet n = do
    length <- randomRIO (0, n)
    set <- sequence $ [randomRIO (0, n) | _ <- [1..length]]
    return $ list2set set 


-- Looking at the type signature for list2set:
--
--   list2set :: Ord a => [a] -> (Set a)
-- 
-- An (Arbitrary [a]) already exist, and since (Gen [a]) is a functor, 
-- we can use the following to obtain a Gen (Set a):
--
--   arbitrary = list2set <$> arbitrary 
-- 
-- to obtain a type Gen (Set a)
--
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary


-- Exercise 3. --
-- =========== --


(∩) :: Ord a => Set a -> Set a -> Set a
(Set els) ∩ b = Set $ filter (`inSet` b) els 

(∪) :: Ord a => Set a -> Set a -> Set a
(Set as) ∪ (Set bs) = list2set (as ++ bs)

(\\\) :: Ord a => Set a -> Set a -> Set a
(Set els) \\\ b = Set $ filter (not . (`inSet` b)) els

intersect, union, difference :: Ord a => Set a -> Set a -> Set a
intersect = (∩)
union = (∪)
difference = (\\\)


-- Some more useful aliases
(⊆), (⊇) :: Ord a => Set a -> Set a -> Bool
a ⊆ b = subSet a b
b ⊇ a = subSet b a

(∈) :: Ord a => a -> Set a -> Bool
a ∈ as = inSet a as 

-- Definitions for intersect, union and difference:
--
-- ∀ x : x ∈ A ∧ x ∈ B <=> x ∈ (A ∩ B)
--
-- ∀ x : x ∈ A ∨ x ∈ B <=> x ∈ (A ∪ B)
--
-- ∀ x : x ∈ A ∧ x ∉ B <=> x ∈ (A \ B)

-- All elements with property, from a given set.
filterSet :: (Ord a) => (a -> Bool) -> Set a -> Set a
filterSet cond (Set xs) = list2set $ filter cond xs


prop_Intersect a@(Set as) b@(Set bs) = (a ∩ b) == filterSet (\x -> x ∈ a && x ∈ b) universe
    where 
        universe = Set $ as ++ bs
        types = (a :: Set Int, b :: Set Int)

prop_Union a@(Set as) b@(Set bs) = (a ∪ b) == filterSet (\x -> x ∈ a || x ∈ b) universe
    where 
        universe = Set $ as ++ bs
        types = (a :: Set Int, b :: Set Int)

prop_Difference a@(Set as) b@(Set bs) = (a \\\ b) == filterSet (\x -> x ∈ a && not(x ∈ b)) universe
    where 
        universe = Set $ as ++ bs
        types = (a :: Set Int, b :: Set Int)

exercise3 = do
    putStrLn "Exercise 3:"
    putStrLn "Intersect:"
    quickCheck prop_Intersect
    putStrLn "Union:"
    quickCheck prop_Union
    putStrLn "Difference:"
    quickCheck prop_Difference


-- Exercise 4. --
-- =========== --



-- Exercise 5. -- (1 hr)
-- =========== --

-- The exercise says type Rel = [(a,a)], but we have a Set definition
-- so we might as well use that. 

type Rel a = Set (a, a)

-- inverseRel R = R^-1
inverseRel :: Ord a => Rel a -> Rel a
inverseRel (Set rs) = list2set $ foldr (\(a,b) is -> (b,a):is) [] rs

symClos :: Ord a => Rel a -> Rel a
symClos r@(Set as) = let (Set bs) = inverseRel r 
                     in list2set (as ++ bs)


-- tests --

-- Properties for symmetric closure S of relation R:
--
-- symmetric:
--      ∀ xRy ∈ S : yRx ∈ S
-- smallest: 
--      ∀ xRy ∈ S : xRy ∈ R ∨ yRx ∈ R
-- superset: 
--      R ⊆ S

prop_SymClosSymmetric r = let s = symClos r
                          in inverseRel s ⊆ s 
    where types = (r :: Rel Int)

prop_SymClosSmallest r = isEmpty (symClos r \\\ r \\\ inverseRel r)
    where types = (r :: Rel Int)

prop_SymClosSuper r = r ⊆ symClos r
    where types = (r :: Rel Int)

exercise5 = do
    putStrLn "Exercise 5:"
    putStrLn "symClos r is symmetric:"
    quickCheck prop_SymClosSymmetric

    putStrLn "symClos r is the smallest symmetric relation:"
    quickCheck prop_SymClosSmallest

    putStrLn "symClos r contains r:"
    quickCheck prop_SymClosSuper

-- Exercise 6. --
-- =========== --

-- Adapted to type Rel a = Set (a, a)

infixr 5 @@
 
(@@) :: (Ord a, Eq a) => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) = list2set [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: (Ord a, Eq a) =>  Rel a -> Rel a
trClos (Set []) = Set []
trClos s@(Set (x:xs)) = let s' = ((s @@ s) ∪ s) in if s' == s then s else trClos s'


-- Properties of the transitive closure S of relation R:
--  
--  ∀ x, y, z: xSy, ySz => xSz
-- 
--  R ⊆ S
--  
--  ∀ S' ⊂ S: S' is not a transitive closure of R

isTransitive :: (Ord a, Eq a) => Rel a -> Bool
isTransitive s@(Set xs) = all (\x -> x ∈ s) [(x, z) | (x, y) <- xs, (y', z) <- xs, y == y']

prop_TrClosTransitive r = isTransitive $ trClos r
    where types = (r :: Rel Int)

prop_TrClosSuperset r = r ⊆ trClos r
    where types = (r :: Rel Int)

prop_TrClosSmallest r@(Set rs) = isSmallest $ trClos r
        where 
            isSmallest s = r ⊆ s && isTransitive s && (not . hasSmaller) s
            hasSmaller (Set []) = False
            hasSmaller (Set [x]) = isSmallest (Set [])
            hasSmaller s@(Set (x:y:xs)) = isSmallest (Set (x:xs)) && isSmallest (Set (y:xs))
            types = (r :: Rel Int)


exercise7 = do
    putStrLn "Exercise 7:"
    putStrLn "Is Transitive:"
    quickCheck prop_TrClosTransitive
    putStrLn "S Contains R:"
    quickCheck prop_TrClosSuperset
    putStrLn "S is minimal:"
    quickCheck prop_TrClosSmallest


-- Exercise 8. --
-- =========== --

{-

Looking at the definitions of transitivity and symmetry:


If R is transitive, then xRy, yRz => xRz. Constructing the symmetric
closure of R then inserts all (zRy, yRx and zRx) for those same elements, 
consequiently, xRy, yRz => xRz still holds for all elements.

If R is symmetrical, then constructing the transitive closure introduces
xRz, zRx for all xRy, yRz and zRy, yRz.   

-}


