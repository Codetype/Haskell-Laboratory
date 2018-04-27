sqr x = x^2

funFactory n = case n of 
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

fact :: Int -> Int
fact n = if n == 0 || n == 1
  then 1
  else n * fact(n-1)

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = \x -> sum[(x^k)/fromIntegral(fact k) | k <- [0..n]]

df :: (Double -> Double) -> Double -> (Double -> Double)
df f h = \x -> (f(x+h) - f(x-h)) / (2*h)
