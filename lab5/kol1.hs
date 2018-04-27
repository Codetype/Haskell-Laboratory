fun = do
    putStrLn "Podaj imie: "
    s <- getLine 
    putStrLn $ "Witaj " ++ s

fun' = putStrLn "Podaj imie: " >>
      getLine >>=
      \s -> putStrLn $ "Witaj " ++ s

data Tree a = Node (Tree a)(Tree a) | Leaf a deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node lt rt) = Node (fmap f lt)(fmap f rt)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node lt rt) = foldMap f lt `mappend` foldMap f rt

fun2 = getLine >>= 
       \x -> return(x ++ x) >>=
       \y -> print [x,y]

fun2' = do
      x <- getLine
      y <- return (x ++ x)
      print [x,y]

fun3 = getLine >>= 
       \l1 -> return (l1 ++ l1) 
       >> putStrLn l1

fun3' = do
       l1 <- getLine
       return (l1 ++ l1)
       putStrLn l1

fun4 = getLine >>= 
       \l1 ->
       getLine >>= 
       \l2 ->
       print [l1, l2]

fun4' = do
       l1 <- getLine
       l2 <- getLine
       print [l1,l2]
