{-# LANGUAGE ScopedTypeVariables #-}
-- import Debug.Trace
import Numeric.Natural

choose :: Eq a => Natural -> [a] -> [[a]]
choose n xs
  | n <= 0 = return []
  | otherwise = do
      x <- xs
      let xs' = [x' | x' <- xs, x /= x' ]
      rest <- choose (n - 1) xs'
      return $ x:rest
