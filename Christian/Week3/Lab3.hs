
module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3



-- Exercise 1: --
-- =========== --

contradiction :: Form -> Bool

contradiction = not . satisfiable

tautology :: Form -> Bool
tautology = contradiction . Neg

entails :: Form -> Form -> Bool
entails a b = tautology $ Impl a b

(|=) = entails

equiv :: Form -> Form -> Bool
equiv a b = (a |= b) && (b |= a)

(<==>) = equiv



satVals :: [Valuation] -> Form -> [Valuation]
satVals vals form = filter (\val -> evl val form) vals

(<|>) :: Eq a => [a] -> [a] -> [a]
(<|>) = union
(<&>) :: Eq a => [a] -> [a] -> [a]
(<&>) = intersect


prop_contradiction form = contradiction form == expected form 
    where
        expected (Prop _) = False
        expected f@(Cnj forms) = filter (\vs -> all (evl vs) forms) (allVals f) == []
        expected f@(Dsj forms) = filter (\vs -> any (evl vs) forms) (allVals f) == []
        expected (Neg a) = all (\vs -> evl vs a) (allVals a)
        expected f@(Impl a b) = filter (\vs -> evl vs a --> evl vs b) (allVals f) == []
        expected f@(Equiv a b) = filter (\vs -> evl vs a == evl vs b) (allVals f) == []

-- Assuming prop_contradiction above is correct, contradiction works
-- as intended.
prop_tautology form = tautology form == contradiction (Neg form)

prop_equiv a b = equiv a b == tautology (Equiv a b)

prop_entails a b = entails a b == tautology (Impl a b)



 
instance Arbitrary Form where
    arbitrary = sized $ \a -> sized $ \b -> genForm (min b 10) a
        
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
                w <- choose (0, size) 
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

    

