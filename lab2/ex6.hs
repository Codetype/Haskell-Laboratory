fib :: (Num a, Eq a) => a -> a
fib n = 
  if n == 0 || n == 1 then 1
  else fib(n-2) + fib(n-1)

--fibs :: Num a => a -> a
fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' []   = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = if x == True
    then True
    else or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = if x == False
    then False
    else and' xs


elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' t (x:xs) = if x == t
    then True
    else elem' t xs

doubleAll :: Num t => [t] -> [t]
doubleAll xs = [2*x | x <- xs]

squareAll :: Num t => [t] -> [t]
squareAll xs = [x^2 | x <- xs]

selectEven :: Integral t => [t] -> [t]
selectEven xs = [x | x <- xs, even x]


