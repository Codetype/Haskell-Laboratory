onlyEven [] = []
onlyEven (x:xs)
  | x `mod` 2 == 0 = x : onlyEven xs
  | otherwise = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
  | x `mod` 2 == 1 = x : onlyOdd xs
  | otherwise = onlyOdd xs

onlyUpper :: [Char] -> [Char]
onlyUpper [] = []
onlyUpper (x:xs)
  | ((fromEnum x > 64) && (fromEnum x < 91)) = x : onlyUpper xs
  | otherwise = onlyUpper xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p  xs

onlyEven' = filter' (\x -> x `mod` 2 == 0)
onlyOdd' = filter' (\x -> x `mod` 2 == 1)
onlyUpper' :: [Char] -> [Char]
onlyUpper' = filter' (\x -> ((fromEnum x > 64) && (fromEnum x < 91)))
