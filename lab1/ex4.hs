sgn :: Int -> Int
sgn n = if n < 0
  then -1
  else if n == 0
    then 0
    else 1
    
absInt :: Int -> Int
absInt x = if x < 0
  then -x
  else x
  
min2Int :: (Int, Int) -> Int
min2Int(x, y) = if x <= y
  then x
  else y
  
min3Int :: (Int, Int, Int) -> Int
min3Int(x, y, z) = if x <= y 
    then if x <= z
      then x
      else z
    else if y <= z
      then y
      else z
    
min3Int2 :: (Int, Int, Int) -> Int
min3Int2(x, y, z) = if x <= y
    then min2Int(x, z)
    else min2Int(y, z)

toUpper :: Char -> Char
toUpper c = toEnum(fromEnum c - 32)

toLower :: Char -> Char
toLower c = toEnum(fromEnum c + 32)

isDigit :: Char -> Bool
isDigit x = if fromEnum x > 47 && fromEnum x < 58
  then True
  else False

charToNum :: Char -> Int
charToNum x = fromEnum x

romanDigit :: Char -> String
romanDigit c = case c of
  '1' -> "I"
  '2' -> "II"
  '3' -> "III"
  '4' -> "IV"
  '5' -> "V"
  '6' -> "VI"
  '7' -> "VII"
  '8' -> "VIII"
  '9' -> "IX"
