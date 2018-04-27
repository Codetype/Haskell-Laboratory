concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concat2 :: [[a]] -> [a]
concat2 = foldr (++) []
