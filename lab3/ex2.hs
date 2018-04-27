sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a->a) -> [a] -> a 
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum1, sumSqr, sumCub, sumAbs :: [Integer] -> Integer
sum1 = sumWith(\e -> e)
sumSqr = sumWith(\e -> e^2)
sumCub = sumWith(\e -> e^3)
sumAbs = sumWith(\e -> abs e)

listLength :: Num a => [a] -> a
listLength = sumWith(\e -> 1)

prod' :: Num a => [a] -> a
prod' [] = 0
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a->a) -> [a] -> a 
prodWith _ [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prod, prodSqr, prodCub, prodAbs :: [Integer] -> Integer
prod = prodWith(\e -> e)
prodSqr = prodWith(\e -> e^2)
prodCub = prodWith(\e -> e^3)
prodAbs = prodWith(\e -> abs e)
