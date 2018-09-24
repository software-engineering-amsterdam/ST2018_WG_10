module Lab3 where

import           Data.List
import           Lecture3
import           System.Random
import           Test.QuickCheck


-- Exercises

-- 1) (60 Min.)

none :: (a -> Bool) -> [a] -> Bool
none f []     = True
none f (x:xs) = (f x == False) && none f xs

contradiction :: Form -> Bool
contradiction f = none (\v -> evl v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f

{-
  Here goes the explanation.
-}


-- Exercise 2 (5 Min.)

parseTest :: Form -> Bool
parseTest f = ([f] :: [Form]) == (parse . show $ f)

{-
  I am missing an `Arbitrary` instance of `Form` for QuickCheck to use here :(
  But generally one could just compare if the `parse` function returns the same
  form that was put into it when casting it into a string beforehand.
  If I can manage to write the `Arbitrary` instance, the function can just be
  run via QuickCheck with: `quickCheck parseTest`.
-}


-- Exercise 3 (120 Min.)

bool2cnf :: Form -> Form
bool2cnf (Prop x)                 = Prop x
bool2cnf (Neg (Prop x))           = Neg (Prop x)
bool2cnf (Cnj fs)                 = Cnj (map bool2cnf fs)
bool2cnf (Dsj [f1, Cnj [f2, f3]]) = Cnj [bool2cnf (Dsj [f1, f2]), bool2cnf (Dsj [f1, f3])]
bool2cnf (Dsj [Cnj [f2, f3], f1]) = Cnj [bool2cnf (Dsj [f1, f2]), bool2cnf (Dsj [f1, f3])]
bool2cnf (Dsj fs)                 = Dsj (map bool2cnf fs)

cnf :: Form -> Form
cnf = bool2cnf . nnf . arrowfree

{-
  The `cnf` function performs a composition on the `bool2cnf` function, composed
  with the `nnf` function, composed with the `arrowfree` function.

  The type signature of `cnf` is `:: Form -> Form`.
  Consequently, the `Form` is first passed into the `arrowfree` function
  which removes implications and equivalences from the `Form`.
  This is necessary owing to the fact that a CNF only consists of conjunctions
  and disjunctions. Moreover, nothing apart from atoms can be negated in a CNF.
  Thus, the `nnf` function is part of the composition as well.
  Finally, the `bool2cnf` takes an non-negated, arrow-free, boolean,
  propositional formula. This input constraint was achieved by composing the
  other two functions. Ergo, the signature of it is `:: Form -> Form` as well.

  The `bool2cnf` function uses four pattern matches for the conversation.
  When given a parametrised `Prop` type, it is directly returned since no
  further processing is needed.
  When a singular conjunction is passed into the function, the `cnf` function
  gets mapped onto the contents of the parametrised type while wrapping the
  result in another conjunction due to the fact that a CNF can only consist
  of conjunctions between clauses.
  The third and fourth pattern matches catch forms that can be converted to a
  CNF by applying the distributive law
  (e.g.: f1 v (f2 ∧ f3) === (f1 v f2) ∧ (f1 v f3)).
  Two different patterns were used to capture both directions of this form
  (the singular atom in front of the conjunction as well as after it).
  The outermost connective is a conjunction while the two inner clauses
  are getting recursively processed after the distributive law was applied.
  If the distributive law cannot be applied, the fifth pattern matches.
  In that case, the contents of the parametrised `Dsj` type are mapped via
  the `bool2cnf` function while wrapping them again in a `Dsj` type.

  Overall, due to its recursive nature, even deeply nested non-CNFs can be
  converted to a valid CNF.
-}


-- Exercise 4
