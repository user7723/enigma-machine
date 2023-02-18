module Enigma.Combinatorics.Common
  ( fac
  ) where

fac :: Integer -> Integer
fac = aux 1
  where
    aux a 0 = a
    aux a 1 = a
    aux a n = aux (a*n) (n-1)
