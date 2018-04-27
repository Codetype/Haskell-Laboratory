not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True
    
absInt n =
  case (n >= 0) of
    True -> n
    _   -> -n
    
isItTheAnswer :: String -> Bool
isItTheAnswer str = case str of
    "Love" -> True
    _      -> False
    
or' :: (Bool, Bool) -> Bool
or' (x, y) = case (x, y) of 
    (False, False) -> False
    _ -> True

and' :: (Bool, Bool) -> Bool
and' (x, y) = case (x, y) of
    (True, True) -> True
    _ -> False
    
nand' :: (Bool, Bool) -> Bool
nand' (x, y) = case (x, y) of
    (True, True) -> False
    _ -> True
    
xor' :: (Bool, Bool) -> Bool
xor' (x, y) = case (x, y) of
    (True, False) -> True
    (False, True) -> True
    _ -> False
