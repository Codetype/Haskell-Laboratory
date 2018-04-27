fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x:y:_) | x == y = True
fst2Eq _ = False

fstDivBy2 :: (Integral a) => [a] -> Bool
fstDivBy2 (x:y:_) | (x `mod` y == 0) = True
fstDivBy2 _ = False

fstDivBy3 :: (Integral a) => [a] -> Bool
fstDivBy3 (x:y:z:_) | (x `mod` z == 0) = True
fstDivBy3 _ = False
