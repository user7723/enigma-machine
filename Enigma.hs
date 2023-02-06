{-# LANGUAGE ScopedTypeVariables #-}
-- import Debug.Trace
import Numeric.Natural

-- This is an illustration of how can one derive a way to compute
-- permutations of size 5 from n-element list where n >= 5
choose5 :: [Natural] -> [[Natural]]
choose5 xs0 = do
  x1 <- xs0
  let xs1 = [ x | x <- xs0, x /= x1 ]
  x2 <- xs1
  let xs2 = [ x | x <- xs1, x /= x2 ]
  x3 <- xs2
  let xs3 = [ x | x <- xs2, x /= x3 ]
  x4 <- xs3
  let xs4 = [ x | x <- xs3, x /= x4 ]
  x5 <- xs4
  return $ x1:x2:x3:x4:x5:[]
