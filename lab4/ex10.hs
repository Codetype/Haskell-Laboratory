data Queue a = Queue [a] deriving (Show)

push :: a -> Queue a -> Queue a
push e (Queue es) = Queue (es ++ [e])

pop :: Queue a -> (a, Queue a)
pop (Queue xs) = (head xs, Queue $ tail xs)

peek :: Queue a -> a
peek (Queue (x:xs)) = x
