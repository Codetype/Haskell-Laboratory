getline' :: IO String
getline' = do
    x <- getChar  
    if x == '\n'
    then return []
    else do
        xs <- getline'
        return (x:xs)
