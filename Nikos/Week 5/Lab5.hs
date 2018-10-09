module Lab5

where 

import Data.List
import System.Random
import Lecture5

-- Exercise 1 (2.5 hours) --

main1 :: IO [()]
main1 = do 
           putStrLn ("\nExample 6 (NRC):")
           solveAndShow2 example6


blocks2 :: [[Int]]
blocks2 = [[2..4],[6..8]]

bl2 :: Int -> [Int]
bl2 x = concat $ filter (elem x) blocks2

showSudoku2 :: Sudoku -> IO()
showSudoku2 = showGrid2 . sud2grid

showGrid2 :: Grid -> IO()
showGrid2 [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+----------+-----------+----------+")
    showRow1 as
    putStrLn ("|   +------|---+   +----------+   |")
    showRow2 bs
    putStrLn ("|   |      |   |   |   |      |   |")
    showRow2 cs
    putStrLn ("+---|------+---|---|---+------|---+")
    showRow2 ds
    putStrLn ("|   +----------+   +----------+   |")
    showRow1 es
    putStrLn ("|   +----------+   +----------+   |")
    showRow2 fs
    putStrLn ("+---|------|---|---|---|------|---|")
    showRow2 gs
    putStrLn ("|   |      |   |   |   |      |   |")
    showRow2 hs
    putStrLn ("|   +----------+   +----------+   |")
    showRow1 is
    putStrLn ("+----------+-----------+----------+")

showRow1 :: [Value] -> IO()
showRow1 [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'
     putStr (showVal a1) ; putStr "    "
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
 do  putChar '|'
     putStr (showVal a1) ; putStr "  | "
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


subGrid2 :: Sudoku -> (Row,Column) -> [Value]
subGrid2 s (r,c) = 
  [ s (r',c') | r' <- bl2 r, c' <- bl2 c ]

freeInSubgrid2 :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid2 s (r,c) = freeInSeq (subGrid2 s (r,c))

freeAtPos2 :: Sudoku -> (Row,Column) -> [Value]
freeAtPos2 s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInSubgrid2 s (r,c))

subgridInjective2 :: Sudoku -> (Row,Column) -> Bool
subgridInjective2 s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid2 s (r,c))

consistent2 :: Sudoku -> Bool
consistent2 s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjective2 s (r,c) | 
                    r <- [2,6], c <- [2,6]]

initNode2 :: Grid -> [Node]
initNode2 gr = let s = grid2sud gr in 
              if (not . consistent2) s then [] 
              else [(s, constraints2 s)]

solveAndShow2 :: Grid -> IO[()]
solveAndShow2 gr = solveShowNs2 (initNode2 gr)

extendNode2 :: Node -> Constraint -> [Node]
extendNode2 (s,constraints2) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune2 (r,c,v) constraints2) | v <- vs ]

prune2 :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune2 _ [] = []
prune2 (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | sameblock2 (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune2 (r,c,v) rest
  | otherwise = (x,y,zs) : prune2 (r,c,v) rest

sameblock2 :: (Row,Column) -> (Row,Column) -> Bool
sameblock2 (r,c) (x,y) = bl2 r == bl2 x && bl2 c == bl2 y 

constraints2 :: Sudoku -> [Constraint] 
constraints2 s = sortBy length3rd 
    [(r,c, freeAtPos2 s (r,c)) | 
                       (r,c) <- openPositions s ]

emptyN2 :: Node
emptyN2 = (\ _ -> 0,constraints2 (\ _ -> 0))

eraseN2 :: Node -> (Row,Column) -> Node
eraseN2 n (r,c) = (s, constraints2 s) 
  where s = eraseS (fst n) (r,c)

randomS2 = genRandomNrcSudoku >>= showNode2

showNode2 :: Node -> IO()
showNode2 = showSudoku2 . fst

succNode2 :: Node -> [Node]
succNode2 (s,[]) = []
succNode2 (s,p:ps) = extendNode2 (s,ps) p

solveShowNs2 :: [Node] -> IO[()]
solveShowNs2 = sequence . fmap showNode2 . solveNs2

solveNs2 :: [Node] -> [Node]
solveNs2 = search succNode2 solved 


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
           if (checkUnique example1) 
               then putStrLn ("Example 1 has unique solution")
               else putStrLn ("Example 1 DOES NOT have a unique solution")
           if (checkUnique example2) 
               then putStrLn ("Example 2 has unique solution")
               else putStrLn ("Example 2 DOES NOT have a unique solution")
           if (checkUnique example3) 
               then putStrLn ("Example 3 has unique solution")
               else putStrLn ("Example 3 DOES NOT have a unique solution")
           if (checkUnique example4) 
               then putStrLn ("Example 4 has unique solution")
               else putStrLn ("Example 4 DOES NOT have a unique solution")
           if (checkUnique example5) 
               then putStrLn ("Example 5 has unique solution")
               else putStrLn ("Example 5 DOES NOT have a unique solution")
           if (checkUnique example6) 
               then putStrLn ("Example 6 has unique solution")
               else putStrLn ("Example 6 DOES NOT have a unique solution")

checkUnique :: Grid -> Bool
checkUnique gr = uniqueSol3 (initNode gr)

uniqueSol3 :: [Node] -> Bool
uniqueSol3 [] = True
uniqueSol3 (x:xs) = uniqueSol x && uniqueSol3 xs

-- Exercise 5 (1 hour) --

main5 :: IO ()
main5 = do r <- genRandomNrcSudoku
           showNode2 r
           s  <- genProblem2 r
           showNode2 s

genRandomNrcSudoku :: IO Node
genRandomNrcSudoku = do [r] <- rsolveNs2 [emptyN2]
                        return r

rsolveNs2 :: [Node] -> IO [Node]
rsolveNs2 ns = rsearch rsuccNode2 solved (return ns)

rsuccNode2 :: Node -> IO [Node]
rsuccNode2 (s,cs) = do xs <- getRandomCnstr cs
                       if null xs 
                          then return []
                          else return 
                            (extendNode2 (s,cs\\xs) (head xs))

genProblem2 :: Node -> IO Node
genProblem2 n = do ys <- randomize xs
                   return (minimalize2 n ys)
   where xs = filledPositions (fst n)

minimalize2 :: Node -> [(Row,Column)] -> Node
minimalize2 n [] = n
minimalize2 n ((r,c):rcs) | uniqueSol2 n' = minimalize2 n' rcs
                         | otherwise    = minimalize2 n  rcs
  where n' = eraseN2 n (r,c)

uniqueSol2 :: Node -> Bool
uniqueSol2 node = singleton (solveNs2 [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False


