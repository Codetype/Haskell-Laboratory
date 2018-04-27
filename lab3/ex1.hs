f1 :: Num a => a -> a
f1 = \x -> x-2

f2 :: Floating a => a -> a -> a 
f2 = \x y -> sqrt(x^2 + y^2)

f3 :: Floating a => a -> a -> a -> a
f3 = \x y z -> sqrt(x^2 + y^2 + z^2)

f4 :: Num a => a -> a
f4 = \x -> 2*x

f5 :: Num a => a -> a
f5 = \x -> x*2

f6 :: (Integral b, Num a) => b -> a
f6 = \x -> 2^x

f7 :: (Integral a, Num a) => a -> a
f7 = \x -> x^2

f8 :: Fractional a => a -> a
f8 = \x -> 2/x

f9 :: Fractional a => a -> a 
f9 = \x -> x/3 

f10 :: Num a => a -> a
f10 = \x -> 4-x

f11 :: Floating a => a -> a
f11 = \x -> sqrt x

f12 :: Num a => a -> a
f12 = \x -> abs x

f13 :: Floating a => a -> a
f13 = \x -> log x

f14 :: a -> a
f14 = \x -> x

f15 :: Integral a => a -> Bool
f15 = \x -> if x `mod` 2 == 0 then True else False

f16 :: Floating a => a -> a
f16 = \x -> let y = sqrt x in 2 * y^3  * (y+1)

f17 :: (Eq a, Num a) => a-> a
f17 = \x -> if x == 1 then 3 else 0


