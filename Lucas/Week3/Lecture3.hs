
module Lecture3

where 

import Data.List
import Data.Char
import Test.QuickCheck
import System.Random
import System.IO.Unsafe

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update 

type Var = String
type Env = Var -> Integer

data Expr = I Integer
          | V Var 
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq,Show)

eval :: Expr -> Env -> Integer 
eval (I i) _ = i 
eval (V name) env = env name
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Subtr e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mult e1 e2) env = (eval e1 env) * (eval e2 env)

assign :: Var -> Expr -> Env -> Env 
assign var expr env =  update env (var, eval expr env)

initEnv :: Env 
initEnv = \ _ -> undefined

initE :: Env
initE = const undefined

example = initEnv $$ 
          assign "x" (I 3) # 
          assign "y" (I 5) # 
          assign "x" (Mult (V "x") (V "y")) #
          eval (V "x")

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

euclid m n = (m,n) $$
   while (\ (x,y) -> x /= y) 
         (\ (x,y) -> if x > y then (x-y,y) 
                              else (x,y-x)) #
         fst

euclid' m n = fst $ eucl (m,n) where
     eucl = until (uncurry  (==))
         (\ (x,y) -> if x > y then (x-y,y) else (x,y-x))

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = while p f # r

euclid2 m n = (m,n) $$
          whiler (\ (x,y) -> x /= y) 
                 (\ (x,y) -> if x > y then (x-y,y) 
                                      else (x,y-x))
                 fst

fibonacci :: Integer -> Integer
fibonacci n = fibon (0,1,n) where
  fibon = whiler 
           (\ (_,_,n) -> n > 0)
           (\ (x,y,n) -> (y,x+y,n-1))
           (\ (x,_,_) -> x)

fb :: Integer -> Integer
fb n = fb' 0 1 n where 
   fb' x y 0 = x 
   fb' x y n = fb' y (x+y) (n-1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving (Eq,Ord)

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3
s = Prop 4

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
form4 = Dsj [p,Neg p]
form5 = Cnj [p, Neg p]
form6 = Dsj [p,q]
form7 = Impl (Equiv p q) (Cnj [(Neg p),r])
formTest = Dsj [Cnj[p,q],r]


propNames :: Form -> [Name]
propNames = sort.nub.pnames where 
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concatMap pnames fs
  pnames (Dsj fs) = concatMap pnames fs
  pnames (Impl f1 f2)  = concatMap pnames [f1,f2]
  pnames (Equiv f1 f2) = concatMap pnames [f1,f2]

type Valuation = [(Name,Bool)]

-- | all possible valuations for lists of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) = 
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- | generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

type ValFct = Name -> Bool

val2fct :: Valuation -> ValFct
val2fct = updates (\ _ -> undefined)

fct2val :: [Name] -> ValFct -> Valuation
fct2val domain f = map (\x -> (x,f x)) domain 

evl :: Valuation -> Form -> Bool
evl [] (Prop c)    = error ("no info: " ++ show c)
evl ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = evl xs (Prop c)
evl xs (Neg f)  = not (evl xs f)
evl xs (Cnj fs) = all (evl xs) fs
evl xs (Dsj fs) = any (evl xs) fs
evl xs (Impl f1 f2) = evl xs f1 --> evl xs f2
evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

data Token 
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv 
      | TokenInt Int 
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs) 
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs 
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form 
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = 
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = 
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) = 
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                         (f2,rest) <- parseImpl ys ]
   ++
  [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                          (f2,rest) <- parseEquiv ys ] 
parseForm tokens = []

parseForms :: Parser Token [Form] 
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens = 
   [(f:fs, rest) | (f,ys) <- parseForm tokens, 
                   (fs,rest) <- parseForms ys ]

parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)


properties = [p,q,r]
negate = [True,False]
operators = [0, 1, 2, 3] --0:Cnj|1:Dsj|2:Impl|3:Equiv

randomList :: Int -> Int -> IO([Int])
randomList 0 ub = return []
randomList n ub = do
  r  <- randomRIO (1,ub)
  rs <- randomList (n-1) ub
  return (r:rs)


--genLogic :: Int -> Form -> Form
--genLogic n _ = do
--                  props <- map (\x -> properties !! x) (randomList n 3)
--                  negs  <- map (\x -> negs !! x) (randomList n 2)
--                  ops   <- randomList n 4


pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)
 
--x <- pick [1, 2, 3]

--drawInt :: Integer -> Integer  -> Integer
--drawInt x y = unsafePerformIO(getStdRandom (randomR (x,y)))


--genLogic :: Form -> Form
--genLogic (Prop x) = if randomIO :: IO Float < 0.25
--                      then Dsj[Prop x, ]if getStdGen<0.25 then genLogic Dsj Cnj Impl or Equiv
--Brackets
--Properties
--negation
--Dsj, Cnj
--Impl, Equiv

--Exercise 1 -Time Spent (1h)

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


ex1 :: IO ()
ex1 = do
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

--A form is satisfiable if there exists at least one combinations of truth values where the entire statement is true
satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

--A form is contradictory if all possible truth value combinations are false
contradiction :: Form -> Bool
contradiction f = all (\ v -> not(evl v f)) (allVals f) --always false

--A form is a tautology if all possible truth value combinations are true
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f) --always true

-- A form has logical entailment if a statement logically follows from another 
entails :: Form -> Form -> Bool
entails f v = tautology (Impl f v)
--all (\g -> if (evl g f) and ((evl g v) then True) else True) (allVals f)

--A form is logically equivalent to another when at each truth value combination the resulting values from the statements are the same
--Or in other words, logical equivalence is equal to a tautology between equivalent statements
equiv :: Form -> Form -> Bool
equiv f v = tautology (Equiv (f) (v))

--This was tested by using a list of examples made up of each category of logical relation 

--Exercise 2 -Time Spent

prop_parse form = parse (show form) == [form]
           where types = (form :: Form)

ex2 :: IO()
ex2 = do
    putStrLn "Test Parse"
    putStr " >  "
    quickCheck prop_parse
    putStrLn ""


--Exercise 3 -Time Spent (2h)

cleanup :: Form -> Form
cleanup (Prop x) = Prop x
cleanup (Neg (Prop x)) = Neg (Prop x)
cleanup (Dsj [f1',Cnj [f2',f3']]) = Cnj (map cleanup [Dsj [f1',f2'], Dsj[f1',f3']]) --Distribution Law
cleanup (Dsj [Cnj [f1',f2'],f3']) = Cnj (map cleanup [Dsj [f1',f3'], Dsj[f2',f3']])
cleanup (Cnj fs) = Cnj (map cleanup fs)
cleanup (Dsj fs) = Dsj (map cleanup fs)


toCNF :: Form -> Form
toCNF = cleanup . nnf . arrowfree

--To test this method a form must remain in the same form when passed through the toCNF function a second time since it is already in CNF form
--so basically if this holds: toCNF form1 == toCNF toCNF form1
--then the CNF conversion is correct
--CHECK DOESNT WORK YET
prop_Is_Correct form = (toCNF . toCNF) form == toCNF form
                where types = (form :: Form)

ex3 :: IO ()
ex3 = do
    putStrLn "Exercise 3:"
    putStr "prop_Is_Correct"
    putStr " >  "
    quickCheck prop_Is_Correct
    putStrLn ""

--Exercise 4 -Time Spent(MANY HOURS)

--Taken from @Christian
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


