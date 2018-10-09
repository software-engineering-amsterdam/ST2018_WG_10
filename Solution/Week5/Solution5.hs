module Solution5 where

import Data.List
import System.Random
import Lecture5

import qualified Exercise2

-- Exercise 1 | Time spent (5h.)
-- ==========

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


exercise1 :: IO()
exercise1 = do
              putStrLn "NRC Example from Lab"
              showSudoku $ grid2sud example1
              putStrLn "SOLVED"
              nrc_solveAndShow example1
              putStrLn "Only one solution found, thus it works :)"


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

--Other random testing examples
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

-- Exercise 2 | Time Spent (4 hours)
-- ==========

{- See Exercise2.hs -}

exercise2 :: IO ()
exercise2 = Exercise2.main


-- Exercise 3 | Time spent: 3 hours
-- ========== 

-- Adapted main so the generated node is returned
genNode :: IO Node
genNode = do 
    [r] <- rsolveNs [emptyN]
    showNode r
    s <- genProblem r
    showNode s
    return $ s


-- Generates a list with one position erased. For each filled position, try to
-- erase it, and test if the result is not unique. If any result is unique, the
-- node is not minimal. 
prop_Minimal :: Node -> Bool
prop_Minimal s = all (not.uniqueSol) [eraseN s p | p <- filledPositions $ fst s]
    
-- The generation is pretty slow, so a small sample size is used.
exercise3 = do
    nodes <- sequence $ replicate 5 genNode
    let allMin = all prop_Minimal nodes 
    putStrLn $ "All are minimal: " ++ show allMin


-- Exercise 4 | Time Spent (4 hours)
-- ==========

{-
sub blocks numbered from 1-9
  _______
  |1|2|3|
  -------
  |4|5|6|
  -------
  |7|8|9|
  -------

  Results:

  It is definetely possible to generate a sudoku with 3 and 4 empty blocks.
  All that needs to be done is to try all combination of blocks, erase them and check whether the solution is unique.
  If it is unique we have found a solution, otherwise try a different combination. 
  If an empty list is returned it means no possible solution has been found. This has been the case for me with 5 empty blocks,
  there might be some possible solutions but my brute force method did not find any.
  As for 4 empty blocks, that seems to be no problem. It generates a solution most of the time, this means that there are some arrangements of sudokus that do not neccesarily have the 4 empty block option
  5 Empty Blocks has not returned a viable solution, this leads me to believe that it is not possible to have this.
-}


--Used to get all combinations of sub-blocks 
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

genBlockProblem :: Node -> Int -> IO Node
genBlockProblem n i = do 
                        bc <- randomize (genSubBlockCombos i)
                        let hd = head bc
                        let tl = tail bc
                        let xs = concat(map getSubBlockPositions (hd))
                        let nod = minimalize n xs
                        if all (==True) (map (isSubBlockEmpty (fst nod)) hd)
                          then return nod
                          else return (runPossibilities n hd tl)

runPossibilities :: Node -> [Int] -> [[Int]] -> Node
runPossibilities n h t = do
                         let hd = head t
                         let tl = tail t
                         let xs = concat(map getSubBlockPositions (hd))
                         let nod = minimalize n xs
                         if all (==True) (map (isSubBlockEmpty (fst nod)) hd)
                           then nod
                           else runPossibilities n hd tl

filledBlocks :: Sudoku -> [(Row,Column)]
filledBlocks s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genSubBlockCombos :: Int -> [[Int]]
genSubBlockCombos n = combinations n [1..9]

eraseBlock :: Sudoku -> [(Row,Column)] -> Sudoku
eraseBlock s l = if (length l == 0) then s
                    else
                      do
                        let h = head l
                        let t = tail l
                        let z = eraseS s h
                        eraseBlock z t

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


exercise4 :: IO ()
exercise4 = do r <- genRandomSudoku
               putStrLn "Random Sudoku, everytime this is run it is different"
               putStrLn "You might have to try a few times to get a problem for 4 Empty Blocks"
               putStrLn "Empty List just means no problem was found for this particular sudoku"
               putStrLn "5 Empty blocks has never returned a problem for me"
               showNode r
               s  <- genBlockProblem r 3
               putStrLn "3 Empty Blocks Problem"
               showNode s
               s  <- genBlockProblem r 4
               putStrLn "4 Empty Blocks Problem"
               showNode s
               s  <- genBlockProblem r 5
               putStrLn "5 Empty Blocks Problem"
               showNode s

-- Exercise 5 | Time Spent (5 hours)
-- ==========

blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) blocksNrc

showSudokuNrc :: Sudoku -> IO()
showSudokuNrc = showGridNrc . sud2grid

showGridNrc :: Grid -> IO()
showGridNrc [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn (" +---------+-----------+----------+")
    showRow1 as
    putStrLn (" |  +------|---+   +----------+   |")
    showRow2 bs
    putStrLn (" |  |      |   |   |   |      |   |")
    showRow2 cs
    putStrLn (" +--|------+---|---|---+------|---+")
    showRow2 ds
    putStrLn (" |  +----------+   +----------+   |")
    showRow1 es
    putStrLn (" |  +----------+   +----------+   |")
    showRow2 fs
    putStrLn (" +--|------|---|---|---|------|---|")
    showRow2 gs
    putStrLn (" |  |      |   |   |   |      |   |")
    showRow2 hs
    putStrLn (" |  +----------+   +----------+   |")
    showRow1 is
    putStrLn (" +---------+-----------+----------+")

showRow1 :: [Value] -> IO()
showRow1 [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putStr " |"
     putStr (showVal a1) ; putStr "   "
     putStr (showVal a2) ; putStr "  "
     putStr (showVal a3) ; putStr " | "
     putStr (showVal a4) ; putStr "   "
     putStr (showVal a5) ; putStr "   "
     putStr (showVal a6) ; putStr " | "
     putStr (showVal a7) ; putStr "  "
     putStr (showVal a8) ; putStr "   "
     putStr (showVal a9) ; putStr " |" ; putChar '\n'

--"|1   2  3 | 4   5   6 | 7  8   9 |"--

showRow2 :: [Value] -> IO()
showRow2 [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putStr " |"
     putStr (showVal a1) ; putStr " | "
     putStr (showVal a2) ; putStr "  "
     putStr (showVal a3) ; putStr " | "
     putStr (showVal a4) ; putStr " | "
     putStr (showVal a5) ; putStr " | "
     putStr (showVal a6) ; putStr " | "
     putStr (showVal a7) ; putStr "  "
     putStr (showVal a8) ; putStr " | "
     putStr (showVal a9) ; putStr " |" ; putChar '\n'

-- |1  |2  3 | 4 | 5 | 6 | 7  8 | 9 | --

{- +---------+-----------+----------+
   |1   2  3 | 4   5   6 | 7  8   9 |
   |   +-----|---+   +----------+   |
   |1  |2  3 | 4 | 5 | 6 | 7  8 | 9 |
   |   |     |   |   |   |      |   |
   |1  |2  3 | 4 | 5 | 6 | 7  8 | 9 |
   +---|-----+---|---|---+------|---+
   |1  |2  3 | 4 | 5 | 6 | 7  8 | 9 |
   |   +---------+   +----------+   |
   |1   2  3 | 4   5   6 | 7  8   9 |
   |   +-----|---+   +---|------+   |
   |1  |2  3 | 4 | 5 | 6 | 7  8 | 9 |
   +---|-----|---|---|---|------|---|
   |1  |2  3 | 4 | 5 | 6 | 7  8 | 9 |
   |   |     |   |   |   |      |   |
   |1  |2  3 | 4 | 5 | 6 | 7  8 | 9 |
   |   +---------+   +----------+   |
   |1   2  3 | 4   5   6 | 7  8   9 |
   +---------+-----------+----------+
                                     -}  

subGridNrc :: Sudoku -> (Row,Column) -> [Value]
subGridNrc s (r,c) = 
  [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNrc s (r,c) = freeInSeq (subGridNrc s (r,c))

freeAtPosNrc :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNrc s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInSubgridNrc s (r,c))

subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where 
   vs = filter (/= 0) (subGridNrc s (r,c))

consistentNrc :: Sudoku -> Bool
consistentNrc s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjectiveNrc s (r,c) | 
                    r <- [2,6], c <- [2,6]]

initNodeNrc :: Grid -> [Node]
initNodeNrc gr = let s = grid2sud gr in 
              if (not . consistentNrc) s then [] 
              else [(s, constraintsNrc s)]

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNsNrc (initNodeNrc gr)

extendNodeNrc :: Node -> Constraint -> [Node]
extendNodeNrc (s,constraintsNrc) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         pruneNrc (r,c,v) constraintsNrc) | v <- vs ]

pruneNrc :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
pruneNrc _ [] = []
pruneNrc (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblockNrc (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | otherwise = (x,y,zs) : pruneNrc (r,c,v) rest

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y 

constraintsNrc :: Sudoku -> [Constraint] 
constraintsNrc s = sortBy length3rd 
    [(r,c, freeAtPosNrc s (r,c)) | 
                       (r,c) <- openPositions s ]

emptyNrc :: Node
emptyNrc = (\ _ -> 0,constraintsNrc (\ _ -> 0))

eraseNrc :: Node -> (Row,Column) -> Node
eraseNrc n (r,c) = (s, constraintsNrc s) 
  where s = eraseS (fst n) (r,c)

randomSNrc = genRandomNrcSudoku >>= showNodeNrc

showNodeNrc :: Node -> IO()
showNodeNrc = showSudokuNrc . fst

succNodeNrc :: Node -> [Node]
succNodeNrc (s,[]) = []
succNodeNrc (s,p:ps) = extendNodeNrc (s,ps) p

solveShowNsNrc :: [Node] -> IO[()]
solveShowNsNrc = sequence . fmap showNodeNrc . solveNsNrc

solveNsNrc :: [Node] -> [Node]
solveNsNrc = search succNodeNrc solved 


exercise5 :: IO ()
exercise5 = do r <- genRandomNrcSudoku
               showNodeNrc r
               s  <- genProblemNrc r
               showNodeNrc s

genRandomNrcSudoku :: IO Node
genRandomNrcSudoku = do [r] <- rsolveNsNrc [emptyNrc]
                        return r

rsolveNsNrc :: [Node] -> IO [Node]
rsolveNsNrc ns = rsearch rsuccNodeNrc solved (return ns)

rsuccNodeNrc :: Node -> IO [Node]
rsuccNodeNrc (s,cs) = do xs <- getRandomCnstr cs
                         if null xs 
                            then return []
                            else return 
                              (extendNodeNrc (s,cs\\xs) (head xs))

genProblemNrc :: Node -> IO Node
genProblemNrc n = do ys <- randomize xs
                     return (minimalizeNrc n ys)
   where xs = filledPositions (fst n)

minimalizeNrc :: Node -> [(Row,Column)] -> Node
minimalizeNrc n [] = n
minimalizeNrc n ((r,c):rcs) | uniqueSolNrc n' = minimalizeNrc n' rcs
                         | otherwise    = minimalizeNrc n  rcs
  where n' = eraseNrc n (r,c)

uniqueSolNrc :: Node -> Bool
uniqueSolNrc node = singleton (solveNsNrc [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False


