import Data.List
sortDesc :: Ord a => [a] -> [a]
sortDesc xs = reverse (sort xs)

sortDesc2 :: Ord a => [a] -> [a]
sortDesc2 xs = reverse $ (sort xs)

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g xs = if map f xs == map g xs then True else False

--infixl 9 >.>
--(>.>) :: (a->b) -> (b->c) -> (a->c)
--g >.> f = 

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
