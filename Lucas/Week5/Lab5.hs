module Lab5 where

import Data.List
import System.Random
import Lecture5

--Exercise 1 | Time spent (5hours)


nrc_blocks :: [[Int]]
nrc_blocks = [[2..4],[6..8]]

nrc_bl :: Int -> [Int]
nrc_bl x = concat $ filter (elem x) nrc_blocks

nrc_subGrid :: Sudoku -> (Row,Column) -> [Value]
nrc_subGrid s (r,c) = 
  [ s (r',c') | r' <- nrc_bl r, c' <- nrc_bl c ]

nrc_subgridInjective :: Sudoku -> (Row,Column) -> Bool
nrc_subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (nrc_subGrid s (r,c))

nrc_sameblock :: (Row,Column) -> (Row,Column) -> Bool
nrc_sameblock (r,c) (x,y) = nrc_bl r == nrc_bl x && nrc_bl c == nrc_bl y 

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

nrc_freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
nrc_freeInSubgrid s (r,c) = freeInSeq (nrc_subGrid s (r,c))

nrc_freeAtPos :: Sudoku -> (Row,Column) -> [Value]
nrc_freeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 
   `intersect` (nrc_freeInSubgrid s (r,c)) 

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

example6 :: Grid
example6 = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

example7 :: Grid
example7 = [[0,0,0,0,0,0,0,8,0],
            [0,0,0,5,0,0,0,0,0],
            [0,0,6,0,0,0,0,0,0],
            [0,4,0,0,2,0,0,0,0],
            [0,0,0,0,4,6,0,0,0],
            [1,0,0,0,0,0,0,3,0],
            [0,0,0,8,0,0,0,9,2],
            [0,0,0,0,0,0,0,0,0],
            [5,6,0,0,0,0,0,0,0]]

example8 :: Grid
example8 = [[0,0,2,0,0,0,0,0,0],
            [0,0,4,0,8,0,9,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,0,0,0,0,5,4,0,1],
            [0,0,0,0,0,0,0,0,0],
            [5,0,0,2,0,0,0,0,8],
            [0,0,0,0,0,6,0,7,0],
            [0,5,0,0,0,0,0,0,0],
            [0,0,0,0,3,0,0,1,0]]