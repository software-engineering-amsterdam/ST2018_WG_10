
module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Debug.Trace
import Lecture3

-- Make it runnable
main :: IO ()
main = do
    exercise1
    exercise2
    exercise3
    exercise4
    exercise5

-- Exercise 1: --
-- =========== --

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology = contradiction . Neg

entails :: Form -> Form -> Bool
entails a b = tautology $ Impl a b

equiv :: Form -> Form -> Bool
equiv a b = (a `entails` b) && (b `entails` a)




satVals :: [Valuation] -> Form -> [Valuation]
satVals vals form = filter (\val -> evl val form) vals

(<|>) :: Eq a => [a] -> [a] -> [a]
(<|>) = union
(<&>) :: Eq a => [a] -> [a] -> [a]
(<&>) = intersect

-- (Prop n) is always satisfiable.
--  > contradiction (Prop n) = False
--
-- (Cnj fs) is a contradiction if for all valuations v there is a subclause that
-- is not fulfilled by v.
--  > contradiction (Cnj fs) = ∀ v ∈ V: (∃ f ∈ fs: ¬(evl v f)
--
-- (Dsj fs) is a contradiction if for all valuations v there is no subclause
-- that is fulfilled by v:
--  > contradiction (Dsj fs) = ∀ v ∈ V: ¬(∃ f ∈ fs: evl v f)
--
-- (Neg f) is a contradiction if f is satisfied for all valuations v:
--  > contradicion (Neg f) =  ∀ v ∈ V: evl v f
--
-- (Impl a b) is a contradiction if there is no valuation that satisfies a -> b:
--  > contradiction (Impl a b) = ∃ v ∈ V: (evl v a -> evl v b)
--
-- (Equiv a b) is a contradiction if there is no valuation that satisfies a <-> b:
--  > contradiction (Equiv a b) = not (∃ v ∈ V: evl v a <-> evl v b)
--
-- To make this work with QuickCheck an instance Arbitrary Form is needed. This
-- is given in exercise 4.
prop_Contradiction form = contradiction form == expected form
    where
        expected (Prop _) = False
        expected f@(Cnj forms) = all (\v -> any (not . evl v) forms) (allVals f)
        expected f@(Dsj forms) = all (\v -> not $ any (evl v) forms) (allVals f)
        expected (Neg f) = all (\vs -> evl vs f) (allVals f)
        expected f@(Impl a b) = not $ any (\vs -> evl vs a --> evl vs b) (allVals f)
        expected f@(Equiv a b) = not $ any (\vs -> evl vs a == evl vs b) (allVals f)


-- Assuming prop_contradiction above is correct, contradiction works as
-- intended, so we can reuse it in other tests. The same is true for tautology.
prop_Tautology form = tautology form == contradiction (Neg form)

-- Again, assuming above tests are correct, tautology is correct, and
-- we can reuse it here.
prop_Equiv a b = equiv a b == tautology (Equiv a b)
prop_Entails a b = entails a b == tautology (Impl a b)


exercise1 :: IO ()
exercise1 = do
    putStrLn "Exercise 1:"
    putStr "prop_Contradiction "
    putStr " >  "
    quickCheck prop_Contradiction
    putStr "prop_Tautology     "
    putStr " >  "
    quickCheck prop_Tautology
    putStr "prop_Equiv         "
    putStr " >  "
    quickCheck prop_Equiv
    putStr "prop_Entails       "
    putStr " >  "
    quickCheck prop_Entails
    putStrLn ""

-- Exercise 2. --


-- Just test whether the parsed string is equal to the original form.
prop_parse form = parse (show form) == [form]
    where types = (form :: Form)

exercise2 :: IO ()
exercise2 = do
    putStrLn "Exercise 2:"
    putStrLn "test parse:"
    putStr " >  "
    quickCheck prop_parse
    putStrLn ""

-- Exercise 3. --


-- Simplified CNF data type
type Atom = Int
type Clause = [Atom]
type Clauses = [Clause]

-- simplifyCnf is used to simplify intermediate results, which drastically
-- improves performance.
simplifyCnf :: Clauses -> Clauses
simplifyCnf cls = sortUniq $ filter (not.isTautology) $ map sortUniq cls
    where
        sortUniq :: (Ord a, Eq a) => [a] -> [a]
        sortUniq = map head . group . sort

        -- tests for occurrences of (a ∨ ¬a ∨ ...)
        isTautology :: [Atom] -> Bool
        isTautology [] = False
        isTautology (a:as) = atomNeg a `elem` as || isTautology as


atomNeg :: Atom -> Atom
atomNeg a = -a

-- distribute the clauses of one CNF formula over the other
distribute :: Clauses -> Clauses -> Clauses
distribute as bs = concat $ map (\b -> distributeClause b as) bs

-- distribute a single clause over the CNF
distributeClause :: Clause -> Clauses -> Clauses
distributeClause b as = map (b++) as

disjCnf :: Clauses -> Clauses -> Clauses
disjCnf as bs = simplifyCnf $ distribute as bs

conjCnf :: Clauses -> Clauses -> Clauses
conjCnf as bs = simplifyCnf (as ++ bs)

negCnf :: Clauses -> Clauses
negCnf as = let cnfs = map (\dj -> simplifyCnf $ map (\a ->  [atomNeg a]) dj) as
            in simplifyCnf $ foldl' disjCnf [[]] cnfs

implCnf :: Clauses -> Clauses -> Clauses
implCnf a b = negCnf a `disjCnf` b

equivCnf :: Clauses -> Clauses -> Clauses
equivCnf a b = implCnf a b `conjCnf` implCnf b a


toCnfClauses :: Form -> Clauses
toCnfClauses (Prop a) = [[a]]
toCnfClauses (Neg (Neg a)) = toCnfClauses a
toCnfClauses (Neg a) = negCnf $ toCnfClauses a
toCnfClauses (Dsj as) = foldl' disjCnf [[]] $ map toCnfClauses as
toCnfClauses (Cnj as) = foldl' conjCnf [] $ map toCnfClauses as
toCnfClauses (Equiv a b) = equivCnf (toCnfClauses a) (toCnfClauses b)
toCnfClauses (Impl a b) = implCnf (toCnfClauses a) (toCnfClauses b)

fromClauses :: Clauses -> Form
fromClauses cnf = Cnj $ map clauseToForm cnf where
    clauseToForm :: Clause -> Form
    clauseToForm as = Dsj $ map atom as
    atom a
        | a < 0 = Neg $ Prop (-a)
        | a > 0 = Prop a
        | otherwise = error "Atom must be non-zero"

formToCnf :: Form -> Form
formToCnf = fromClauses . toCnfClauses

-- test if the CNF formula is equivalent to the original formula
prop_CnfIsEquiv form = equiv (formToCnf form) form

-- test if the result is CNF
prop_Cnf form = iscnf $ formToCnf form
    where
        iscnf (Cnj xs) = all isclause xs
        iscnf _ = False

        isclause (Dsj xs) = all isatom xs
        isclause _ = False

        isatom (Neg (Prop _)) = True
        isatom (Prop _) = True
        isatom _ = False


-- CNF is CNF, so repeated application should be equivalent to a  single
-- application
prop_IsIdempotent form = (formToCnf . formToCnf) form == formToCnf form

exercise3 :: IO ()
exercise3 = do
    putStrLn "Exercise 3:"
    putStr "prop_CnfIsEquiv  "
    putStr " >  "
    quickCheck prop_CnfIsEquiv
    putStr "prop_Cnf         "
    putStr " >  "
    quickCheck prop_Cnf
    putStr "prop_IsIdempotent"
    putStr " >  "
    quickCheck prop_IsIdempotent
    putStrLn ""

-- Exercise 4. --

instance Arbitrary Form where
    arbitrary = do
        n <- choose(1, 5)
        m <- choose(n, 2 * n) --the size is limited because toCnfClauses is difficult
        genForm n m

        where
            genForm :: Int -> Int -> Gen Form
            genForm nprop 0 = genProp nprop 0
            genForm nprop size = oneof $ map (\gen -> gen nprop size) clauseGens

            genNeg :: Int -> Int -> Gen Form
            genNeg nprop size = Neg <$> genForm nprop (size - 1)

            genProp :: Int -> Int -> Gen Form
            genProp nprop size = Prop <$> choose (1, nprop)

            genList :: Int -> Int -> Gen [Form]
            genList nprop size = do
                w <- choose (2, size)
                sequence $ [genForm nprop (size `quot` w) | _ <- [1..w]]

            genDsj :: Int -> Int -> Gen Form
            genDsj nprop size = Dsj <$> genList nprop size

            genCnj :: Int -> Int -> Gen Form
            genCnj nprop size = Cnj <$> genList nprop size

            genImpl :: Int -> Int -> Gen Form
            genImpl nprop size = Impl <$> genForm nprop (size `quot` 2) <*> genForm nprop (size `quot` 2)

            genEquiv :: Int -> Int -> Gen Form
            genEquiv nprop size = Equiv <$> genForm nprop (size `quot` 2) <*> genForm nprop (size `quot` 2)

            clauseGens :: [Int -> Int -> Gen Form]
            clauseGens = [genNeg, genProp, genDsj, genCnj, genImpl, genEquiv]

exercise4 :: IO ()
exercise4 = do
    putStrLn "Exercise 4:"
    putStrLn "Example generated forms:"
    example <- generate arbitrary :: IO Form
    putStr " >  "
    putStrLn $ show $ example
    example <- generate arbitrary :: IO Form
    putStr " >  "
    putStrLn $ show $ example
    putStrLn ""

-- Bonus: exercise 5 -- (10 minutes)
-- This was already done to convert Forms to CNF form. Using the [[Int]] type
-- to define the CNF has the advantage that type correctness proves that the
-- result is a conjunction of disjunctions, whereas the Form type could be any
-- formula.

newtype CnfForm = CnfForm Form deriving (Show)

instance Arbitrary (CnfForm)
    where
        arbitrary = CnfForm <$> formToCnf <$> arbitrary

cnf2cls :: Form -> Clauses
cnf2cls = toCnfClauses

-- Test if the Clauses representation is equivalent to the Form representation
prop_CnfClausesEquiv (CnfForm form) = prop_Cnf form ==>
                    (fromClauses $ cnf2cls form) `equiv` form

exercise5 :: IO ()
exercise5 = do
    putStrLn "Exercise 5 (bonus):"
    putStr " >  "
    quickCheck prop_CnfClausesEquiv
