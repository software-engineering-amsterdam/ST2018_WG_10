module Lab6

where

import Lecture6

-- Exercise 1 --

my_exM :: Integer -> Integer -> Integer -> Integer
my_exM x y n
    | y == 1 = x `mod` n
    | even y = (my_exM x (y `div` 2) n)^2 `mod` n
    | odd y = (x * (my_exM x (y-1) n)) `mod` n

-----------------------------------------

-- Exercise 2 --

-- 
