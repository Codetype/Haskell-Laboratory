isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = all(\(x,y) -> x <= y) $ zip xs (tail xs)

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = all(\(x,y) -> x >= y) $ zip xs (tail xs)

everySecond :: [t] -> [t]
everySecond xs = [n | (i,n) <- zip [1..] xs, i `mod` 2 == 1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWIth' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x,y) : xys) = (x:xs, y:ys)
  where (xs, ys) = unzip' xys

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ [] = []
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

unzip3' :: [(a,b,c)] -> ([a], [b], [c])
unzip3' [] = ([], [], [])
unzip3' ((x,y,z) : xys) = (x:xs, y:ys, z:zs)
  where (xs, ys, zs) = unzip3' xys


