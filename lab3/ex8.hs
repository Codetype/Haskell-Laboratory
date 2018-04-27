sumWith g [] = 0 
sumWith g (x:xs) = g x + sumWith g xs

prodWith g [] = 1
prodWith g (x:xs) = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
  where
    go acc g [] = acc
    go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
  where
    go acc g [] = acc
    go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' g = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl' :: (b->a->b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs 

sumWith''' g = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 1

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x xs -> f x:xs) []

map'' :: (a->b) -> [a] -> [b]
map'' f = foldl (\xs x -> f x:xs) []

filter' :: (a->Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x:xs else xs) []

filter'' :: (a->Bool) -> [a] -> [a]
filter'' f = foldl (\xs x -> if f x then x:xs else xs) []

foldl'' :: (a->b->a) -> a -> [b] -> a
foldl'' f a bs = foldr (\b g x -> g (f x b)) id bs a

foldr'' :: (b -> a -> a) -> a -> [b] -> a
foldr'' f a bs = foldl (\g b x -> g (f b x)) id bs a
