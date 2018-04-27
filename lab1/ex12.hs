{-

let f1 i = if 2 > 3 then True else False 
f1 :: t -> Bool

let f1 i = if i > 3 then True else False 
f1 :: (Num a, Ord a) => a -> Bool


let f1(x,y) = x==y
f1 :: Eq a => (a, a) -> Bool

let f1(x,y) = x == 4
f1 :: (Eq a, Num a) => (a, t) -> Bool

let f1(x,y) = if x /= 0 then 1/x else 0
f1 :: (Eq a, Fractional a) => (a, t) -> a

let f1(x,y) = if x /= 0 then 1/y else 0
f1 :: (Eq a, Fractional a1, Num a) => (a, a1) -> a1

let f1(x,y) = if x >= 0 then 1/x else 
f1 :: (Fractional a, Ord a) => (a, t) -> a

-}
