(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b->c) -> (a->b) -> (a->c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.> 

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

exctractMaybe :: Maybe a -> a
exctractMaybe Nothing = error "Nothing inside!"
exctractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe = Just

-- (>^$>) = exctract (^) and apply ($)
(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
ma >^$> f = (exctractMaybe ma) >$> f
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x+1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10*x) else Nothing


(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
--f >.>> g = \x -> g (exctractMaybe (f x)) 
f >.>> g = \x ->  (f x) >^$> g
--f >.>> g = \x -> joinMaybe fmap g (f x) 

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just (Just a)) = Just a
joinMaybe (Just Nothing)  = Nothing
joinMaybe Nothing         = Nothing
