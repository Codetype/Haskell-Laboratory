import Control.Monad.Trans.Writer.Lazy

gcdWithLog :: Int -> Int -> Writer [String] Int
gcdWithLog a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcdWithLog b (a `mod` b)

mapWithLog :: Show a => (a -> b) -> [a] -> Writer [String] [b]
mapWithLog _ [] = do
  tell ["map []"]
  return []
mapWithLog f (x:xs) = do
  tell ["map " ++ show x]
  mapXs <- mapWithLog f xs
  return $ f x : mapXs

filterWithLog :: Show a => (a -> Bool) -> [a] -> Writer [String] [a]
filterWithLog _ [] = do
  tell ["filter "]
  return [] 
filterWithLog f (x:xs) = do
  tell ["filter " ++ show x]
  filterXs <- filterWithLog f xs
  return $ if f x then x : filterXs else filterXs
