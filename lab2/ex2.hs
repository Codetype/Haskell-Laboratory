fiveToPower :: Integer -> Integer
fiveToPower = (5^)

toPower5 :: Num a => a->a
toPower5 = (^5)

substrNFrom5 :: Num a => a->a
substrNFrom5 = (5-)

subtr5From :: Num a => a->a
subtr5From = flip2 (-) 5

flip2 :: (a->b->c) -> b -> a -> c
flip2 f x y = f y x

flip3 :: (a->b->c->d) -> b->a->c->d
flip3 f x y z = f y x z
