--length [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2+b^2==c^2]


isPrime :: Integral t => t -> Bool
isPrime n = if n<2
    then False
    else [i | i <- [2..n-1], n `mod` i == 0] == []

--length [x | x <- [1..1000], isPrime x ]

primes :: [Int]
primes = eratoSieve [2..]
  where
    eratoSieve :: [Int] -> [Int]
    eratoSieve (p:xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

--isPrime' :: Integral t => t -> Bool
--isPrime' m = if eratoSieve m == m 
  --then True
  --else False
    --where
      --eratoSieve :: [Int] -> [Int]
      --eratoSieve (p:xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

allEquals :: Eq a => [a] -> Bool
allEquals [] = True
allEquals [x] = True
allEquals (x:y:xs) = x == y && allEquals(y:xs)
