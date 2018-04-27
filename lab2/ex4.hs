isPalindrome :: [Char] -> Bool
isPalindrome s = if (s == reverse s)
    then True
    else False

getElemAtIdx :: Int -> [Char] -> Char
getElemAtIdx a s = head (drop a s)

toUpper :: Char -> Char
toUpper c | ((fromEnum c > 96) && (fromEnum c < 123))  = toEnum(fromEnum c - 32)
    | otherwise = c

capitalize :: [Char] -> [Char]
capitalize w = toUpper(head w) : (tail w)
