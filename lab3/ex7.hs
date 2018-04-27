doubleElems [] = []
doubleElems (x:xs) = 2*x : doubleElems xs

sqrElems [] = []
sqrElems (x:xs) = x^2 : sqrElems xs

lowerCase :: [Char] -> [Char]
lowerCase [] = []
lowerCase (x:xs) = toEnum(fromEnum x+32) : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' = map' (\e -> 2*e)
sqrElems' = map' (\e -> e^2)
lowerCase' :: [Char] -> [Char]
lowerCase' = map (\e -> toEnum(fromEnum e+32))



evalFuncAtList :: a -> [a -> b] -> [b]
evalFuncAtList x [] = []
evalFuncAtList x (f:fs) = f x : evalFuncAtList x fs

