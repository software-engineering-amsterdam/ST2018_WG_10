module Lab5 where

import Data.List
import System.Random
import Lecture5

--Exercise 1 | Time spent (5h.)

--Defined the NRC blocks for extra constraints
nrc_blocks :: [[Int]]
nrc_blocks = [[2..4],[6..8]]

--Returns the block in which the input parameter x is located
nrc_bl :: Int -> [Int]
nrc_bl x = concat $ filter (elem x) nrc_blocks

--Returns the block to which a tuple of coordinates belong
nrc_subGrid :: Sudoku -> (Row,Column) -> [Value]
nrc_subGrid s (r,c) = 
  [ s (r',c') | r' <- nrc_bl r, c' <- nrc_bl c ]

--Checks whether the given subgrid is injective
nrc_subgridInjective :: Sudoku -> (Row,Column) -> Bool
nrc_subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (nrc_subGrid s (r,c))

--Checks if two coordinate tuples are in the same NRC block
nrc_sameblock :: (Row,Column) -> (Row,Column) -> Bool
nrc_sameblock (r,c) (x,y) = nrc_bl r == nrc_bl x && nrc_bl c == nrc_bl y 

--Checks whether the whole puzzle is consistent
nrc_consistent :: Sudoku -> Bool
nrc_consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ nrc_subgridInjective s (r,c) | --Constraints for subgrids added
                    r <- [2,6], c <- [2,6]]

--Checks whether a position is still free in an NRC subgrid
nrc_freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
nrc_freeInSubgrid s (r,c) = freeInSeq (nrc_subGrid s (r,c))

--Checks the available values at a certain position
nrc_freeAtPos :: Sudoku -> (Row,Column) -> [Value]
nrc_freeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 
   `intersect` (nrc_freeInSubgrid s (r,c)) 

--Creates an initial node from a grid
nrc_initNode :: Grid -> [Node]
nrc_initNode gr = let s = grid2sud gr in 
              if (not . nrc_consistent) s then [] 
              else [(s, nrc_constraints s)]


nrc_solveAndShow :: Grid -> IO[()]
nrc_solveAndShow gr = nrc_solveShowNs (nrc_initNode gr)

nrc_solveShowNs :: [Node] -> IO[()]
nrc_solveShowNs = sequence . fmap showNode . nrc_solveNs

nrc_succNode :: Node -> [Node]
nrc_succNode (s,[]) = []
nrc_succNode (s,p:ps) = nrc_extendNode (s,ps) p 

nrc_solveNs :: [Node] -> [Node]
nrc_solveNs = search nrc_succNode solved

nrc_extendNode :: Node -> Constraint -> [Node]
nrc_extendNode (s,nrc_constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         nrc_prune (r,c,v) nrc_constraints) | v <- vs ]

nrc_constraints :: Sudoku -> [Constraint] 
nrc_constraints s = sortBy length3rd 
    [(r,c, nrc_freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

nrc_prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
nrc_prune _ [] = []
nrc_prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : nrc_prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : nrc_prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : nrc_prune (r,c,v) rest
  | nrc_sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : nrc_prune (r,c,v) rest
  | otherwise = (x,y,zs) : nrc_prune (r,c,v) rest

--Example from the lab
example1 :: Grid
example1 = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

example2 :: Grid
example2 = [[0,0,0,0,0,0,0,8,0],
            [0,0,0,5,0,0,0,0,0],
            [0,0,6,0,0,0,0,0,0],
            [0,4,0,0,2,0,0,0,0],
            [0,0,0,0,4,6,0,0,0],
            [1,0,0,0,0,0,0,3,0],
            [0,0,0,8,0,0,0,9,2],
            [0,0,0,0,0,0,0,0,0],
            [5,6,0,0,0,0,0,0,0]]

example3 :: Grid
example3 = [[0,0,2,0,0,0,0,0,0],
            [0,0,4,0,8,0,9,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,0,0,0,0,5,4,0,1],
            [0,0,0,0,0,0,0,0,0],
            [5,0,0,2,0,0,0,0,8],
            [0,0,0,0,0,6,0,7,0],
            [0,5,0,0,0,0,0,0,0],
            [0,0,0,0,3,0,0,1,0]]

example4 :: Grid
example4 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8],
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

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


--Exercise 2 | Time Spent (1:30)

type Position = (Row,Column)
type Constrnt = [[Position]]

type Node' = (Sudoku, Constrnt, [Constraint])

positionz :: [Position]
positionz = [(r,c) | r <- positions, c <- positions]

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]

std_constrnts :: Constrnt
std_constrnts = concat [rowConstrnt, columnConstrnt, blockConstrnt]

nrc_constrnts = std_constrnts ++ [[(r, c) | r <- [x..x+2], c <- [y..y+2]] |
                                x <- [2,6],
                                y <- [2,6]]

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let
    ys = filter (elem (r,c)) xs
  in
    if ys == [] then [1..9] else
      foldl1 intersect (map ((values \\) . map s) ys)

initNode' :: Constrnt -> Grid -> [Node']
initNode' c g = let s = grid2sud g in
                if (not . constrntInjective' c) s then []
                else [(s, c, constraints' s c)]

openPositions' :: Sudoku -> [(Row,Column)]
openPositions' s = [ (r,c) | (r, c) <- positionz, s (r,c) == 0 ]

constraints' :: Sudoku -> Constrnt -> [Constraint]
constraints' s co = sortBy length3rd
    [(r,c, freeAtPos' s (r,c) co) |
                       (r,c) <- openPositions' s ]

constrntInjective' :: Constrnt -> Sudoku -> Bool
constrntInjective' cs s = all injective [filter (/=0) $ map s p | p <- cs]

consistent' :: Sudoku -> Bool
consistent' s = constrntInjective' std_constrnts s

showNode' :: Node' -> IO()
showNode' (s, _, _) = showSudoku s

solveAndShow' :: Grid -> IO[()]
solveAndShow' = solveAndShowRef' std_constrnts

solved'  :: Node' -> Bool
solved' (s, cst, x) = null x && constrntInjective' cst s

extendNode' :: Node' -> Constraint -> [Node']
extendNode' (s,cst,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     cst,
     sortBy length3rd $
         prune' cst (r,c,v) constraints) | v <- vs ]

prune' :: Constrnt -> (Row,Column,Value)
      -> [Constraint] -> [Constraint]
prune' _ _ [] = []
prune' cst (r,c,v) ((x,y,zs):rest) = let
        cst' = filter (elem (r, c)) cst
    in
        if any (elem (x, y)) cst' then
            (x, y, zs \\ [v]) : prune' cst (r, c, v) rest
        else
            (x, y, zs) : prune' cst (r, c, v) rest

succNode' :: Node' -> [Node']
succNode' (_,_,[]) = []
succNode' (s,c,p:ps) = extendNode' (s,c,ps) p

solveAndShowRef' :: Constrnt -> Grid -> IO [()]
solveAndShowRef' c g = solveShowNs' (initNode' c g)

solveShowNs' :: [Node'] -> IO[()]
solveShowNs' = sequence . fmap showNode' . solveNs'

solveNs' :: [Node'] -> [Node']
solveNs' = search succNode' solved'

comp_test_nrc :: Grid -> IO ()
comp_test_nrc g = do
    putStrLn "OG"
    nrc_solveAndShow g
    putStrLn "Refactored"
    let (s, cst, _) = head $ solveNs' $ initNode' nrc_constrnts g
    showSudoku s
    putStr "NRC Consistent: "
    putStrLn $ show $ constrntInjective' cst s

comp_test :: Grid -> IO [()]
comp_test g = do
    putStrLn "OG"
    solveAndShow g
    putStrLn "Refactored"
    solveAndShow' g

--Exercise 3 | Time Spent ()

--Used to get all combinations of sub-blocks 
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']


choose n list = concatMap permutations $ choose' list [] where
  choose' []     r = if length r == n then [r] else []
  choose' (x:xs) r | length r == n = [r]
                   | otherwise     = choose' xs (x:r) 
                                  ++ choose' xs r

{-
sub blocks numbered from 1-9
  _______
  |1|2|3|
  -------
  |4|5|6|
  -------
  |7|8|9|
  -------


  Pseudocode:

  --Obtain complete sudoku
  --Get random combinations of sub blocks to leave empty
  --randomize that list
  --Go through the list leaving sub blocks
  --Take away elements one by one, check if the sudoku is 
  --
-}


eraseS' :: Sudoku -> (Row,Column) -> Sudoku
eraseS' s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN' :: Node -> (Row,Column) -> Node
eraseN' n (r,c) = (s, constraints s) 
  where s = eraseS' (fst n) (r,c) 

minimalize' :: Node -> [(Row,Column)] -> [Int]-> Node
minimalize' n [] sb= n
minimalize' n ((r,c):rcs) sb | all (==0) (map isSubBlockEmpty sb) = minimalize' n' rcs
                             | otherwise    = minimalize' n  rcs
  where n' = eraseN' n (r,c)

filledPositions' :: Sudoku -> [(Row,Column)]
filledPositions' s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

--Returns all the filled blocks
filledBlocks :: Sudoku -> [(Row,Column)]
filledBlocks s = concat $ map getSubBlockPositions (head $ genSubBlockCombos 3)

genProblem' :: Node -> IO Node
genProblem' n = do
                  ys <- randomize xs
                  return (minimalize' n ys (head $ genSubBlockCombos 3))
                where xs = filledBlocks (fst n) --Make this to contain the subblocks as FilledPositions 

--sb <- randomize $ genSubBlockCombos 3
--sb' <- map getSubBlockPositions (head sb)

blocksToEmpty :: Sudoku -> [(Row,Column)]
blocksToEmpty s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genSubBlockCombos :: Int -> [[Int]]
genSubBlockCombos n = combinations n [1..9]

isSubBlockEmpty :: Sudoku -> Int -> Bool
isSubBlockEmpty s n = all (==0) (map s (getSubBlockPositions n))

getSubBlockPositions :: Int -> [(Row,Column)]
getSubBlockPositions 1 = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
getSubBlockPositions 2 = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
getSubBlockPositions 3 = [(1,7),(1,8),(1,9),(2,7),(2,8),(2,9),(3,7),(3,8),(3,9)]
getSubBlockPositions 4 = [(4,1),(4,2),(4,3),(5,1),(5,2),(5,3),(6,1),(6,2),(6,3)]
getSubBlockPositions 5 = [(4,4),(4,5),(4,6),(5,4),(5,5),(5,6),(6,4),(6,5),(6,6)]
getSubBlockPositions 6 = [(4,7),(4,8),(4,9),(5,7),(5,8),(5,9),(6,7),(6,8),(6,9)]
getSubBlockPositions 7 = [(7,1),(7,2),(7,3),(8,1),(8,2),(8,3),(9,1),(9,2),(9,3)]
getSubBlockPositions 8 = [(7,4),(7,5),(7,6),(8,4),(8,5),(8,6),(9,4),(9,5),(9,6)]
getSubBlockPositions 9 = [(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]