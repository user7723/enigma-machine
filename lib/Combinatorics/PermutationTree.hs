{-# LANGUAGE ScopedTypeVariables #-}

module Combinatorics.PermutationTree
  ( nthPermutation
  ) where

--import Numeric.Integer
import Data.List (delete)

type NthPerm   = Integer

data Branch a = Branch a (Tree a)
  deriving Show

type Tree a = [Branch a]

permute :: Eq a => [a] -> Tree a
permute ns = do
  n <- ns
  let ns' = delete n ns
  return $ Branch n (permute ns')

-- to choose 5th permutation in a tree you need sequentially
-- do the following
-- 5 -> -- input
--   5 `mod` 3 = |2| -- index of a particular node in a list of branches
--   5 `div` 3 = 1   -- nth permutation for next sub-tree
-- 1 ->
--   1 `mod` 3 = |1|
--   1 `div` 3 = 0
-- 0 -> ... -- same logic, until there is no more choices left,
--          -- i.e the list of branches is empty

-- Permutations are enumerated starting from zero
--  0         1st        2nd
--  |         |          |
--  v         v          v
-- |1₀|,      |2₁|,      |3₂|
-- |2₀|,|3₁|  |1₀|,|3₁|  |1₀|,|2₁|
-- |3₀| |2₀|  |3₀| |1₀|  |2₀| |1₀|
--      ^          ^          ^
--      |          |          |
--      3rd        4th        5th

-- nthPermutation function can be used to represent the factory issued
-- rotor of the Enigma machine with specific serial number.
-- It's main feature is that it computes all possible permutations
-- lazily, so we can safely traverse it by following specific paths
-- without redundant computations, making its operation relatively fast
-- At most it will evaluate n-spines of a List of Branches times the
-- height of the constructed tree which is log n (total: n * log n)
-- But, for our purposes of modeling Enigma n is relatively small
-- and is not assumed to grow so we can think that it actually
-- does the computation in constant time
nthPermutation :: forall a. Eq a => NthPerm -> [a] -> [a]
nthPermutation n = aux n . permute
  where
    aux :: NthPerm -> Tree a -> [a]
    aux _ [] = []
    aux np bs =
      let (np', i) = np `divMod` l
          (Branch x bs') = bs !! (fromIntegral i) -- unsafe
          l = fromIntegral $ length bs -- length of next sub-tree
      in x : aux np' bs'
