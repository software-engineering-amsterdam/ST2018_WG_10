module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Debug.Trace
import Lecture3


-- Exercises

-- 1)

-- 2)

-- 3)

-- 4)

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
