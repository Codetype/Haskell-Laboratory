qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
  where 
    leftPart xs = [ y | y <- xs, y <= x]
    rightPart xs = [ y | y <- xs, y > x]


qSort2 :: Ord a => [a] -> [a]
qSort2 [] = []
qSort2 (x:xs) = qSort2 (leftPart xs) ++ [x] ++ qSort2 (rightPart xs)
  where 
    leftPart xs = filter (<= x) xs 
    rightPart xs = filter (> x) xs 

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs) 

reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where
    rev (x:xs) acc = rev xs (x:acc)
    rev [] acc = acc

zip' :: [a]->[b]->[(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 
zip' _ _   = []

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((a,b):xs) = (a : as, b : bs)
    where (as,bs) = unzip' xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs
zip3' _ _ _ = []
