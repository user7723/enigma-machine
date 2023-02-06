{-# LANGUAGE ScopedTypeVariables #-}
-- import Debug.Trace
import Numeric.Natural

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (delete, inits)

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

type ListIndex = Integer
type ListSize = Integer
type NthPerm = Integer


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
    len   = fromIntegral $ length xs :: Integer
    nmod  = shrink len n             :: Integer

    shrink :: Integer -> Integer -> Integer
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

fac :: Integer -> Integer
fac n
  | n <= 0    = 1
  | otherwise = n * fac (n - 1)

type SerialNumber = Integer
type Rotor = Map Natural (Natural, Natural)

rotorSize :: Natural
rotorSize = 26

-- the right and left sides of the Rotor are ordered numerically
-- and correspond to each other in geometric sense
-- it's their commutation that is mixed
nthFactoryRotor :: SerialNumber -> Rotor
nthFactoryRotor s = 
  let alph = [0 .. rotorSize - 1]
  in M.fromList
      $ zip alph
      $ (\x -> zip x x) (nthPermutation s alph)

type Level = Int
type Obj   = Natural
type Exc = [Obj]
type State = [Obj]

data PTree = Fst Obj [PTree]
           | Snd Obj [PTree]
           | Nil
  deriving Show

data Switch = R | F | S
  deriving Show

-- This function builds up a tree of all possible pairing combinations
-- given a list of even length. The point is to model non-deterministic
-- choice out of many possibilities, by choosing only one specific
-- branch of a tree, which is possible due to lazy evaluation model
-- The total amount of possible pairs is computed as:
-- "n! / 2*(n-2)!" where n is the length of an input list
pairTree :: [Obj] -> PTree
pairTree []     = Nil
pairTree (x:xs) = aux [] R x 0 xs
  where
    aux :: Exc -> Switch -> Obj -> Level -> [Obj] -> PTree
    aux _ S o l [] = Snd o []
    aux _ _ _ _ [] = Nil
    aux _ R o l xs =
      Fst o $ nodes S l $ noExclusion xs
    aux es F o l xs =
      let xs' = exclude es xs
      in if (es == xs)
         then Nil
         else Fst o $ nodes S l $ noExclusion xs
    aux _ S o l xs =
      let es = inits xs
      in Snd o $ nodes F l $ zip es xs

    noExclusion  = zip (repeat [])
    exclude es   = filter (not . (`elem` es))
    nodes s l xs = [aux e s x (l+1) (delete x (snd $ unzip xs)) | (e, x) <- xs]
