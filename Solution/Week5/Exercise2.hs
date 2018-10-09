
module Exercise2

where

import qualified Lecture5 as L5
import Data.List
import System.Random
import Debug.Trace
import Data.Time.Clock
import qualified Data.Map as Map

{- 
This module contains the refactored code for Exercise 2. 

The following changes were made:
    
1. freeAtPos was replaced by freeAtPos', as suggested in the assignment. 

2. The Node type was changed to:
  
   > type Node = (Sudoku, Constrnt, [Constraint])

   This allows for easy extensibility, the same code can run both NRC and 
   normal sudokus, simply by passing a different constraint set to the solver.
   The functions succNode and initNode have been adapted accordingly. 

3. prune has been modified to:
  
   > prune :: Constrnt -> (Row, Col, Value) -> [Constraint] -> [Constraint]

   Just like the original implementation, prune applies the constraints to the
   Constraint values, eliminating values that, together with the given value, 
   would violate the constraint. 


Extending the solver with NRC constraints is easy in the refactored code:

    solveAndShowWithConstrnt :: Constrnt -> Grid -> IO [()]

This function can take any Constrnt, without any modification to the code. 


Performance testing was implemented in perfTest :: IO ().
The tests are done using the old solver L5.solveAndShow, and the
refactored solver solveAndShow, which is measured using the Data.Time.Clock module.

The refactored solver is about 2 times slower:
 
>>> perfTest
> (...)
> Original: 1.6119573s
> Refactored: 3.9775735s


-}





type Row    = Int
type Column = Int
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9]

-- positions2 = positions × positions
positions2 :: [(Int, Int)]
positions2 = [(r, c) | r <- positions, c <- positions]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s =
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ]

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c)
  where
  pos :: [[a]] -> (Row,Column) -> a
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

type Position = (Row, Column)
type Constrnt = [[Position]]

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]


constrnts :: Constrnt
constrnts = concat [rowConstrnt, columnConstrnt, blockConstrnt]

nrcConstrnts = constrnts ++ [[(r, c) | r <- [m..m+2], c <- [n..n+2]] |
                                n <- [2,6],
                                m <- [2, 6]]

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let
        ys = filter (elem (r,c)) xs
    in
        if ys == [] then 
            values 
        else
            foldl1 intersect (map ((values \\) . map s) ys)


injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

constrntInjective :: Constrnt -> Sudoku -> Bool
constrntInjective cs s = all injective [filter (/=0) $ map s p | p <- cs]

consistent :: Sudoku -> Bool
consistent s = constrntInjective constrnts s

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend s v = update s v

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (y,z) x = if x == y then z else f x

type Constraint = (Row,Column,[Value])

type Node = (Sudoku, Constrnt, [Constraint])


showNode :: Node -> IO()
showNode (s, _, _) = showSudoku s

solved  :: Node -> Bool
solved (s, cst, x) = null x && constrntInjective cst s

extendNode :: Node -> Constraint -> [Node]
extendNode (s,cst,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     cst,
     sortBy length3rd $
         prune cst (r,c,v) constraints) | v <- vs ]

prune :: Constrnt -> (Row,Column,Value)
      -> [Constraint] -> [Constraint]
prune _ _ [] = []
prune cst (r,c,v) ((x,y,zs):rest) = let
        cst' = (constraintsOf cst) (r, c)
    in
        if any (elem (x, y)) cst' then
            (x, y, zs \\ [v]) : prune cst (r, c, v) rest
        else
            (x, y, zs) : prune cst (r, c, v) rest

-- The constraints that apply to a certain cell
constraintsOf :: Constrnt -> Position -> Constrnt
constraintsOf cst (x, y) = filter (elem (x, y)) cst

initNode :: Constrnt -> Grid -> [Node]
initNode cst gr = let s = grid2sud gr in
                  if (not . constrntInjective cst) s then []
                  else [(s, cst, constraints s cst)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | (r, c) <- positions2, s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraints :: Sudoku -> Constrnt -> [Constraint]
constraints s cst = sortBy length3rd
    [(r,c, freeAtPos' s (r,c) cst) |
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node])
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs)
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved

succNode :: Node -> [Node]
succNode (_,_,[]) = []
succNode (s,c,p:ps) = extendNode (s,c,ps) p

solveAndShow :: Grid -> IO[()]
solveAndShow = solveAndShowWithCnstrnt constrnts

solveAndShowWithCnstrnt :: Constrnt -> Grid -> IO [()]
solveAndShowWithCnstrnt cst gr = solveShowNs (initNode cst gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8],
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

-- source: 
-- https://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
example6 :: Grid
example6 = [[8,0,0,0,0,0,0,0,0],
            [0,0,3,6,0,0,0,0,0],
            [0,7,0,0,9,0,2,0,0],
            [0,5,0,0,0,7,0,0,0],
            [0,0,0,0,4,5,7,0,0],
            [0,0,0,1,0,0,0,3,0],
            [0,0,1,0,0,0,0,6,8],
            [0,0,8,5,0,0,0,1,0],
            [0,9,0,0,0,0,4,0,0]]

main :: IO ()
main = do
    putStrLn "Example 3:"
    solveAndShow example3
    putStrLn "Example NRC:"
    let (s, cst, _) = head $ solveNs $ initNode nrcConstrnts example5
    showSudoku s
    putStr "NRC Consistent: "
    putStrLn $ show $ constrntInjective cst s
    perfTest


doTestCase :: (Grid -> IO a) ->  IO ()
doTestCase f = do
    sequence $ map (\(i, g) -> putStrLn (show i)  >> f g) $ zip [1..] testcases
    return ()
        where testcases = [example1, example2, example3, example6]

perfTest :: IO ()
perfTest = do
    t0 <- getCurrentTime
    -- the original
    doTestCase L5.solveAndShow
    t1 <- getCurrentTime
    -- the refactored
    doTestCase solveAndShow    
    t2 <- getCurrentTime
    putStrLn $ "Original: " ++ show (diffUTCTime t1 t0)
    putStrLn $ "Refactored: " ++ show (diffUTCTime t2 t1)


