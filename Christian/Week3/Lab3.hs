
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
-- =========== --

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
-- =========== --

-- Simplified CNF data type
type Atom = Int
type Clause = [Atom]
type Clauses = [Clause]

toCnf :: Form -> Clauses
toCnf = toCnfClauses . negToAtoms . implToDsj
    where
        -- Rewrite bi-implication as conjunction of implications,
        -- followed by implications as disjunction:
        -- a <-> b ==> a -> b ∧ b -> a
        -- a -> b ==> ¬a ∨ b
        implToDsj :: Form -> Form
        implToDsj (Prop a) = Prop a
        implToDsj (Neg f) = Neg $ implToDsj f
        implToDsj (Dsj fs) = Dsj $ map implToDsj fs
        implToDsj (Cnj fs) = Cnj $ map implToDsj fs
        implToDsj (Equiv f g) = implToDsj $ Cnj [Impl f g, Impl g f]
        implToDsj (Impl f g) = implToDsj $ Dsj [Neg f, g]

        -- Apply De Morgan's laws to move negations to atoms,
        -- eliminate double negations
        negToAtoms :: Form -> Form
        negToAtoms (Prop f) = Prop f
        negToAtoms (Neg (Prop f)) = Neg (Prop f)
        negToAtoms (Neg (Cnj fs)) = negToAtoms $ Dsj $ map Neg fs
        negToAtoms (Neg (Dsj fs)) = negToAtoms $ Cnj $ map Neg fs
        negToAtoms (Neg (Neg f)) = negToAtoms f
        negToAtoms (Dsj fs) = Dsj $ map negToAtoms fs
        negToAtoms (Cnj fs) = Cnj $ map negToAtoms fs
        negToAtoms a = error $ "Unexpected formula: " ++ show a

        -- Simplify the CNF to make it run a bit faster:
        -- Eliminate duplicate atoms: (a ∨ a) |= a
        -- Eliminate duplicate clauses: (a ∨ b) ∧ (a ∨ b) |= (a ∨ b)
        -- Eliminate tautologies: (a ∨ ¬a ∨ b) |= T
        -- Eliminate contradictions: (a ∨ b) ∧ (¬b) |= a
        simplify :: Clauses -> Clauses
        simplify = fc . map head . group . sort . eliminateTautologies . map simplifyClause
            where
                fc xs
                    | filterContradictions xs == xs = xs
                    | otherwise = fc $ filterContradictions xs

                filterContradictions :: Clauses -> Clauses
                filterContradictions cs = map (\c -> c \\ units cs) cs
                    where
                        units = map (negate . head) . filter ((==1) . length)

                simplifyClause :: Clause -> Clause
                simplifyClause = map head . group . sort

                isTautology :: Clause -> Bool
                isTautology [] = False
                isTautology (c:cs) = -c `elem` cs || isTautology cs

                eliminateTautologies :: Clauses -> Clauses
                eliminateTautologies = filter (not . isTautology)

        -- Convert formula of Dsj, Cnj and Atoms to CNF
        toCnfClauses :: Form -> Clauses
        toCnfClauses = simplify . toCnfClauses' where
            toCnfClauses' (Prop a) = [[a]]
            toCnfClauses' (Neg (Prop a)) = [[-a]]
            toCnfClauses' (Dsj as) = foldl' disjCnf [[]] $ map toCnfClauses as
            toCnfClauses' (Cnj as) = foldl' conjCnf [] $ map toCnfClauses as
            toCnfClauses' a = error $ "Unexpected formula: " ++ show a

        -- distribute the clauses of one CNF formula over the other
        distribute :: Clauses -> Clauses -> Clauses
        distribute as bs = concat $ map (\b -> distributeClause b as) bs

        -- distribute a single clause over the CNF
        distributeClause :: Clause -> Clauses -> Clauses
        distributeClause b as = map (b++) as

        disjCnf :: Clauses -> Clauses -> Clauses
        disjCnf as bs = distribute as bs

        conjCnf :: Clauses -> Clauses -> Clauses
        conjCnf = (++)


fromClauses :: Clauses -> Form
fromClauses cnf = Cnj $ map clauseToForm cnf where
    clauseToForm :: Clause -> Form
    clauseToForm as = Dsj $ map atom as
    atom a
        | a < 0 = Neg $ Prop (-a)
        | a > 0 = Prop a
        | otherwise = error "Atom must be non-zero"

formToCnf :: Form -> Form
formToCnf = fromClauses . toCnf

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


exercise3 :: IO ()
exercise3 = do
    putStrLn "Exercise 3:"
    putStr "prop_CnfIsEquiv  "
    putStr " >  "
    quickCheck prop_CnfIsEquiv
    putStr "prop_Cnf         "
    putStr " >  "
    quickCheck prop_Cnf
    putStrLn ""

-- Exercise 4. -- (1 hr)
-- =========== --

instance Arbitrary Form where
    arbitrary = do
        n <- choose(1, 12)
        m <- choose(n, 4 * n)
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
-- ================= --
-- This was already done to convert Forms to CNF form. Using the [[Int]] type
-- to define the CNF has the advantage that type correctness proves that the
-- result is a conjunction of disjunctions, whereas the Form type could be any
-- formula.

newtype CnfForm = CnfForm Form deriving (Show)

instance Arbitrary CnfForm
    where
        arbitrary = CnfForm <$> formToCnf <$> arbitrary

cnf2cls :: Form -> Clauses
cnf2cls = toCnf

-- Test if the Clauses representation is equivalent to the Form representation
prop_CnfClausesEquiv (CnfForm form) = prop_Cnf form ==>
                    (fromClauses $ cnf2cls form) `equiv` form

exercise5 :: IO ()
exercise5 = do
    putStrLn "Exercise 5 (bonus):"
    putStr " >  "
    quickCheck prop_CnfClausesEquiv



