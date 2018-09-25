module Lab3 where

import           Data.List
import           Debug.Trace
import           Lecture3
import           System.Random
import           Test.QuickCheck


-- Exercises

-- 1)

none :: (a -> Bool) -> [a] -> Bool
none f []     = True
none f (x:xs) = (f x == False) && none f xs

contradiction :: Form -> Bool
contradiction f = none (\v -> evl v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f


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


-- 2)

{-
  Generally one could just compare if the `parse` function returns the same
  form that was put into it when casting it into a string beforehand.
-}

parseTest :: Form -> Bool
parseTest f = ([f] :: [Form]) == (parse . show $ f)

parseCheck :: IO ()
parseCheck = quickCheck parseTest


-- 3)

-- Simplified CNF data type
type Atom = Int
type Clause = [Atom]
type Clauses = [Clause]

toCnf :: Form -> Clauses
toCnf = toCnfClauses . nnf . arrowfree
    where
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

        -- Distribute the clauses of one CNF formula over the other
        distribute :: Clauses -> Clauses -> Clauses
        distribute as bs = concat $ map (\b -> distributeClause b as) bs

        -- Distribute a single clause over the CNF
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

-- Test if the CNF formula is equivalent to the original formula
prop_CnfIsEquiv form = equiv (formToCnf form) form

-- Test if the result is CNF
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


{-
  The `cnf` function performs a composition on the `bool2cnf` function, composed
  with the `nnf` function, composed with the `arrowfree` function.

  The type signature of `cnf` is `:: Form -> Form`.
  Consequently, the `Form` is first passed into the `arrowfree` function
  which removes implications and equivalences from the `Form`.
  This is necessary owing to the fact that a CNF only consists of conjunctions
  and disjunctions. Moreover, nothing apart from atoms can be negated in a CNF.
  Thus, the `nnf` function is part of the composition as well.
  Finally, the `bool2cnf` takes an non-negated, arrow-free, boolean,
  propositional formula. This input constraint was achieved by composing the
  other two functions. Ergo, the signature of it is `:: Form -> Form` as well.

  The `bool2cnf` function uses four pattern matches for the conversation.
  When given a parametrised `Prop` type, it is directly returned since no
  further processing is needed.
  When a singular conjunction is passed into the function, the `cnf` function
  gets mapped onto the contents of the parametrised type while wrapping the
  result in another conjunction due to the fact that a CNF can only consist
  of conjunctions between clauses.
  The third and fourth pattern matches catch forms that can be converted to a
  CNF by applying the distributive law
  (e.g.: f1 v (f2 ∧ f3) === (f1 v f2) ∧ (f1 v f3)).
  Two different patterns were used to capture both directions of this form
  (the singular atom in front of the conjunction as well as after it).
  The outermost connective is a conjunction while the two inner clauses
  are getting recursively processed after the distributive law was applied.
  If the distributive law cannot be applied, the fifth pattern matches.
  In that case, the contents of the parametrised `Dsj` type are mapped via
  the `bool2cnf` function while wrapping them again in a `Dsj` type.

  Overall, due to its recursive nature, even deeply nested non-CNFs can be
  converted to a valid CNF.
-}


-- 4)

instance Arbitrary Form where
    arbitrary = do
        n <- choose(1, 5)
        m <- choose(n, 2 * n) -- The size is limited because toCnfClauses is difficult
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


-- 5) (Bonus)

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
