module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


chisquared :: Integral a => [a] -> [a] -> Float
chisquared hyp obs = sum . map (uncurry chisq) $ zip hyp obs
    where
        chisq ex ob = let o = fromIntegral ob
                          e = fromIntegral ex
                      in ((o - e) ^ 2 / e)

probsCheck :: IO ()
probsCheck = do
    obs <- probs 10000
    bins <- pure $ map (truncate . (*4)) obs
    counts <- pure $ foldr (\n cs -> take n cs ++ (cs !! n)+1:(drop (n + 1) cs)) [0,0,0,0] bins
    expected <- pure [2500, 2500, 2500, 2500]
    chisq <- pure $ chisquared expected counts
    if chisq < 16.27 then
        putStrLn "No significant deviation found. (p > 0.1%)"
    else
        putStrLn $ "Found deviation from expected value: " ++ show counts ++ " /= " ++ show expected ++ ", Chi sq.: " ++ show chisq ++ " (p < 0.1%)"

