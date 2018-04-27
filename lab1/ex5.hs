absInt :: Int -> Int
absInt n | n >= 0 = n
  | otherwise = -n
  
sgn :: Int -> Int
sgn n | n > 0 = 1
    | n == 0 = 0
    | otherwise = -1
    
min2Int :: (Int, Int) -> Int
min2Int(x,y) | x < y = x
    | otherwise = y  

min3Int :: (Int, Int, Int) -> Int
min3Int(x,y,z) | x < min2Int(y,z) = x
    | otherwise = min2Int(y,z)

toUpper :: Char -> Char
toUpper c | ((fromEnum c > 96) && (fromEnum c < 123))  = toEnum(fromEnum c - 32)
    | otherwise = c

toLower :: Char -> Char
toLower c | ((fromEnum c > 64) && (fromEnum c < 91))  = toEnum(fromEnum c + 32)
    | otherwise = c

charToNum :: Char -> Int
charToNum x = fromEnum x

isDigit :: Char -> Bool
isDigit x | fromEnum x > 47 && fromEnum x < 58 = True
    | otherwise = False

romanDigit :: Char -> String
romanDigit c | c == '1' = "I"
 | c ==  '2' = "II"
 | c ==  '3' = "III"
 | c ==  '4' = "IV"
 | c ==  '5' = "V"
 | c ==  '6' = "VI"
 | c ==  '7' = "VII"
 | c ==  '8' = "VIII"
 | c ==  '9' = "IX"
 | otherwise = "Bledne dane"
