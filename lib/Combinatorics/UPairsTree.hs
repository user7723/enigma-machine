module Combinatorics.UPairsTree
  (getNthPairCombination
  ) where

import Combinatorics.Common

import Numeric.Natural
import Data.List (delete)

type Level = Int
type Obj   = Natural

data PTree = Node Obj [PTree]
  deriving Show

data Switch = F | S
  deriving (Show, Eq)

-- This function builds up a tree of all possible pairing combinations
-- given a list of even length. The point is to model non-deterministic
-- choice out of many possibilities, by choosing only one specific
-- branch of a tree, which is possible due to lazy evaluation model
-- The total amount of possible pairs is computed as:
-- n! / m! * 2^m
--   where n - length of an input list
--         m - n / 2
-- The formula was stolen from:
-- https://www.physicsforums.com/threads/number-of-pairings-in-a-set-of-n-objects.668904/
-- It works in the following way:
--   there is n! ways to permute n objects without repetition
--   m is the amount of pairs we can get out of n objects - n/2.
--   Pairs in that tree of permutations may be arranged in m! ways
--   and for each pair we want to exclude their commutation, so
--   for each m pairs we divide by 2, thus we have 2^m

pairsTree :: [Obj] -> PTree
pairsTree []     = error "empty list"
pairsTree (x:xs) = aux F x 0 xs
  where
    aux :: Switch -> Obj -> Level -> [Obj] -> PTree
    aux S o l [] = Node o []
    aux _ _ _ [] = error "there is an odd amount of objects"
    aux S o l (x:xs) = Node o [aux F x (l+1) xs]
    aux _ o l xs = Node o [aux S x (l+1) (delete x xs) | x <- xs]

countBranches :: PTree -> Natural
countBranches (Node _ ts)
  | null ts = 1
  | otherwise = sum $ map countBranches ts

combsOfDistinctUPairs :: Natural -> Natural
combsOfDistinctUPairs n =
  let m = n `div` 2
  in fac n `div` (fac m * 2 ^ m)

numToPairTreeBranch n i
  | odd n = error "odd number of objects"
  | n < 2 = error "number of objects must be at least 2"
  | otherwise =
      let gs   = if n >= 4 then [n-2, n-4 .. 2] else []
          ns   = map combsOfDistinctUPairs gs
          maxi = combsOfDistinctUPairs n
          i'   = i  `mod` maxi
      in aux i' ns
  where
    -- there is always only one possible way to choose the last pair
    aux _ [] = [0, 0]
    -- the extra zero is because first members of pairs are always fixed
    aux n (x:xs) =
      let (q,r) = n `divMod` x
      in 0 : q : aux r xs

getNthPairCombination :: [Obj] -> Natural -> [(Obj,Obj)]
getNthPairCombination os i =
  let l = fromIntegral $ length os -- amount of objs
      t = [pairsTree os]
      b = numToPairTreeBranch l i
  in aux b t
  where
    aux [] _        = []
    aux (i:[]) _    = error "should have been even number of indices"
    aux (f:s:is) tr =
      let (Node a tr')  = tr !! (fromIntegral f)
          (Node b tr'') = tr' !! (fromIntegral s)
      in (a,b) : aux is tr''
