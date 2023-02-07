{-# LANGUAGE ScopedTypeVariables #-}
-- import Debug.Trace
import Numeric.Natural
import Data.Tuple (swap)

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (delete, inits)

import Control.Monad (guard)

choose :: Eq a => Natural -> [a] -> [[a]]
choose n xs
  | n <= 0 = return []
  | otherwise = do
      x <- xs
      let xs' = [x' | x' <- xs, x /= x' ]
      rest <- choose (n - 1) xs'
      return $ x:rest

data Branch a = Branch a (Tree a)
  deriving Show

type Tree a = [Branch a]

permute :: Eq a => [a] -> Tree a
permute [] = []
permute ns = do
  n <- ns
  let ns' = [n' | n' <- ns, n' /= n]
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

type ListIndex = Natural
type ListSize = Natural
type NthPerm = Natural


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
nthPermutation n xs = aux len nmod perms
  where
    perms = permute xs               :: Tree a
    len   = fromIntegral $ length xs :: Natural
    nmod  = shrink len n             :: Natural

    shrink :: Natural -> Natural -> Natural
    shrink l idx =
      let upBound = fac len
      in if idx >= upBound
         then idx `mod` upBound
         else idx

    aux :: ListSize -> NthPerm -> Tree a -> [a]
    aux _ _ [] = []
    aux l np bs =
      let i   = np `mod` l :: ListIndex
          np' = np `div` l :: NthPerm -- for next sub-tree
          (Branch x bs') = bs !! (fromIntegral i) -- unsafe
          l' = fromIntegral $ length bs' -- length of next sub-tree
      in x : aux l' np' bs'

fac :: Natural -> Natural
fac n
  | n <= 0    = 1
  | otherwise = n * fac (n - 1)

type SerialNumber = Natural
type Rotor = Map Natural (Natural, Natural)

rotorSize :: Natural
rotorSize = 26

enumedPins :: [Pin]
enumedPins = [0 .. rotorSize - 1]

-- the right and left sides of the Rotor are ordered numerically
-- and correspond to each other in geometric sense
-- it's their commutation that is mixed
nthFactoryRotor :: SerialNumber -> Rotor
nthFactoryRotor s =
  M.fromList
    $ zip enumedPins
    $ (\x -> zip x x) (nthPermutation s enumedPins)

type Level = Int
type Obj   = Natural
type Exc = [Obj]
type State = [Obj]

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

pairTree :: [Obj] -> PTree
pairTree []     = error "empty list"
pairTree (x:xs) = aux F x 0 xs
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
      t = [pairTree os]
      b = numToPairTreeBranch l i
  in aux b t
  where
    aux [] _        = []
    aux (i:[]) _    = error "should have been even number of indices"
    aux (f:s:is) tr =
      let (Node a tr')  = tr !! (fromIntegral f)
          (Node b tr'') = tr' !! (fromIntegral s)
      in (a,b) : aux is tr''

type Pin = Natural
type Reflector = Map Pin Pin

getReflectorBySerialNumber :: SerialNumber -> Reflector
getReflectorBySerialNumber s =
  let ps = getNthPairCombination enumedPins s
  in foldr (\p acc -> insert (swap p) (insert p acc)) M.empty ps
  where
    insert p = uncurry M.insert p
