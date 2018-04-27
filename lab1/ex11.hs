{-
Typy w Haskellu

-Num
(+) (-) (*) negate abs signum fromInteger

-Num a => Fractional a where
(/) recip fromRational

-Eq
(==) (/=)

-Eq a => Ord a where
compare (<) (<=) (>) (>=) max min
-}

{-
1)
let f1 x = x
f1 :: t -> t

2)
let f2 x = True
f2 :: t -> Bool

3)
let f3(x,y) = x+y
f3 :: Num a => (a,a) -> a

4)
let f4(x,y) = x/y
f4 :: Fractional a => (a,a) -> a

5)
let f5(x,y) = x /= y
f5 :: Eq a => (a,a) -> Bool


6)
let f6(x,y) = x > y
f6 :: Ord a => (a,a) -> Bool

7)
let f7 (x,y) = if x > y then x + y else x/4
f7 :: (Fractional a, Ord a) => (a, a) -> a 

-}
--kartk1
--1)
sgn :: Int -> Int
sgn x | x > 0 = 1
      | x == 0 = 0
      | otherwise = -1
--2)

absInt :: Int -> Int
absInt x | x >= 0 = x
         | otherwise = -x

--3
f2 x = if x == 1 
  then 3
  else if x == 2
    then 10
      else 1 

{-
let f3 i = if 2>3 then True else False
f3 :: a -> Bool

let f4(x,y) = x == y 
f4 :: Eq a => (a,a) -> Bool

let f5(x,y) = if x /= 0 then 1/x else 0
f5 :: (Eq a, Fractional a) => (a, t) -> a


-}
