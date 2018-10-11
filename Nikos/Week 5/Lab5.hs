module Lab5

where 

import Data.List
import System.Random
import Lecture5

-- Exercise 1 (2.5 hours) --

main1 :: IO [()]
main1 = do 
           putStrLn ("\nExample 6 (NRC):")
           solveAndShowNrc example6


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


-- Exercise 3 (2 hours) --

{- checkUnique checks if every empty cell of a given sudoku
   has a unique solution and if at least one of them has more
   than one values that can take then the function returns false
   else it returns true. 

-}


main3 :: IO ()
main3 = do 
           putStrLn ("--------------------------------------------")
           putStrLn ("|Checking examples with normal sudoku rules|")
           putStrLn ("--------------------------------------------")
           if ((checkUnique example1) && (isMinimal example1)) 
               then putStrLn ("Example 1 is minimal")
               else do 
                      putStrLn ("Example 1 is NOT minimal.")
                      if (checkUnique example1)
                      then do putStrLn ("One minimalized version would be:")
                              showMinimalized example1
                      else pure()
           if ((checkUnique example2) && (isMinimal example2)) 
               then putStrLn ("Example 2 is minimal")
               else do 
                      putStrLn ("Example 2 is NOT minimal.")
                      if (checkUnique example2)
                      then do putStrLn ("One minimalized version would be:")
                              showMinimalized example2
                      else pure()
           if ((checkUnique example3) && (isMinimal example3)) 
               then putStrLn ("Example 3 is minimal")
               else do 
                      putStrLn ("Example 3 is NOT minimal.")
                      if (checkUnique example3)
                      then do putStrLn ("One minimalized version would be:")
                              showMinimalized example3
                      else pure()
           if ((checkUnique example5) && (isMinimal example5)) 
               then putStrLn ("Example 5 is minimal")
               else do 
                      putStrLn ("Example 5 is NOT minimal.")
                      if (checkUnique example5)
                      then do putStrLn ("One minimalized version would be:")
                              showMinimalized example5
                      else pure()
           if ((checkUnique example6) && (isMinimal example6)) 
               then putStrLn ("Example 6 is minimal")
               else do 
                      putStrLn ("Example 6 is NOT minimal.")
                      if (checkUnique example6)
                      then do putStrLn ("One minimalized version would be:")
                              showMinimalized example6
                      else pure()


checkUnique :: Grid -> Bool
checkUnique gr = hasUniqueSol (initNode gr)

hasUniqueSol :: [Node] -> Bool
hasUniqueSol [] = True
hasUniqueSol (x:xs) = uniqueSol x && hasUniqueSol xs

gridToNode :: Grid -> Node
gridToNode gr = r
         where [r] = initNode gr

showMinimalized :: Grid -> IO ()
showMinimalized x = showNode (minimalize mn (filledPositions (fst mn)))
                  where mn = gridToNode x 

isMinimal :: Grid -> Bool
isMinimal x = (checkUnique x) && compareSudoku mn (minimalize mn (filledPositions (fst mn)))
                where mn = gridToNode x 

compareSudoku :: Node -> Node -> Bool
compareSudoku x y = filledPositions (fst x) == filledPositions (fst y)

showExample :: Grid -> IO ()
showExample x = showNode $ gridToNode x


-- Exercise 5 (1,5 hour) --

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


