module Combinatorics.Common
  ( fac
  , choose
  ) where

import Numeric.Natural

choose :: Eq a => Natural -> [a] -> [[a]]
choose n xs
  | n == 0 = return []
  | otherwise = do
      x <- xs
      let xs' = [x' | x' <- xs, x /= x' ]
      rest <- choose (n - 1) xs'
      return $ x:rest

fac :: Natural -> Natural
fac = aux 1
  where
    aux a 0 = a
    aux a 1 = a
    aux a n = aux (a*n) (n-1)

