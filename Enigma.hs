{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

-- import Debug.Trace
import Numeric.Natural
import Data.Tuple (swap)

import Data.Map (Map, (!))
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
type Offset       = Natural
type StateNumber  = Natural
type Symbol       = Natural
type Pin          = Natural

type Reflector    = Map Pin Pin
-- There is two maps because we need routes to go in
-- opposite direction after reflection
data Rotor = Rotor
  { rLeft  :: Map Pin Pin
  , rRigth :: Map Pin Pin
  } deriving Show

data RotorSt = RotorSt
  { rOffset :: Offset
  , rotor   :: Rotor
  } deriving Show

data Magazine = Magazine
  { getR3  :: RotorSt
  , getR2  :: RotorSt
  , getR1  :: RotorSt
  , mState :: StateNumber
  } deriving Show

data Enigma = Enigma
  { reflector :: Reflector
  , magazine  :: Magazine
  } deriving Show

rotorSize :: Pin
rotorSize = 26

pins :: [Pin]
pins = [0 .. rotorSize - 1]

-- the right and left sides of the Rotor are ordered numerically
-- and correspond to each other in geometric sense
-- it's their commutation that is mixed
nthFactoryRotor :: SerialNumber -> Rotor
nthFactoryRotor s = Rotor
  (M.fromList $ zip pins ps)
  (M.fromList $ zip ps pins)
  where
    ps = nthPermutation s pins


initNthRotor :: SerialNumber -> RotorSt
initNthRotor s = RotorSt 0 (nthFactoryRotor s)

rotateRotor :: RotorSt -> (Bool, RotorSt)
rotateRotor (RotorSt o r) =
  let o' = f o
  in (o' < o, RotorSt o' r)
  where f = (`mod` rotorSize) . (+1)

setRotorState :: Offset -> RotorSt -> RotorSt
setRotorState o (RotorSt _ r) = RotorSt o r

incStateNumber :: StateNumber -> StateNumber
incStateNumber s = (s + 1) `mod` rotorSize^3

splitState :: StateNumber -> (Offset,Offset,Offset)
splitState s =
  let s0 = s `mod` (rotorSize^3)
      (s1,x1) = s0 `divMod` rotorSize
      (s2,x2) = s1 `divMod` rotorSize
      (_ ,x3) = s2 `divMod` rotorSize
  in (x1,x2,x3)

setMagazineState :: StateNumber -> Magazine -> Magazine
setMagazineState s Magazine{..} =
  let (x1,x2,x3) = splitState s
  in Magazine
    { getR1 = setRotorState x1 getR1
    , getR2 = setRotorState x2 getR2
    , getR3 = setRotorState x3 getR3
    , mState = s
    }

initMagazine :: SerialNumber -> SerialNumber -> SerialNumber -> Magazine
initMagazine s1 s2 s3 = Magazine
  { getR1 = initNthRotor s1
  , getR2 = initNthRotor s2
  , getR3 = initNthRotor s3
  , mState = 0
  }

initEnigma :: Reflector -> Magazine -> StateNumber -> Enigma
initEnigma r m sn = Enigma r (setMagazineState sn m)

adjustEnigmaState :: Enigma -> StateNumber -> Enigma
adjustEnigmaState e sn = e { magazine = (setMagazineState sn m) }
  where m = magazine e


-- It's pretty safe to use it
-- [mapWithOffset o1 o2 p | o1 <- pins, o2 <- pins, p <- pins]
mapWithOffset :: Offset -> Offset -> Pin -> Pin
mapWithOffset o1 o2 p = fromIntegral $
  (rsi + (o2i - o1i) + pi) `mod` rsi
  where
    rsi = fromIntegral rotorSize :: Integer
    pi  = fromIntegral p         :: Integer
    o1i = fromIntegral o1        :: Integer
    o2i = fromIntegral o2        :: Integer


enigma =
  initEnigma
    (nthFactoryReflector 0)
    (initMagazine 12933993 912391293 2939232)
    0

data RotorSide = L | R

passRot :: RotorSt -> RotorSide -> Offset -> Symbol -> (Symbol, Offset)
passRot rs lr o1 s =
  let p = mapWithOffset o1 o2 s
  in  (r ! p, o2)
  where
    (o2,r) = case lr of
      L -> (rOffset rs, rLeft $ rotor rs)
      R -> (rOffset rs, rRigth $ rotor rs)

getRotors :: Magazine -> (RotorSt, RotorSt, RotorSt)
getRotors m = (getR1 m, getR2 m, getR3 m)

passMag :: Magazine -> RotorSide -> Symbol -> Symbol
passMag m lr s =
  let (s1,o1) = passRot r1 lr 0 s
      (s2,o2) = passRot r2 lr o1 s1
      (s3,o3) = passRot r3 lr o2 s2
  in mapWithOffset o3 0 s3
  where
    rs@(r1', r2', r3') = getRotors m
    (r1, r2, r3) = case lr of
      L -> (r3', r2', r1')
      _ -> rs

right2left :: Magazine -> Symbol -> Symbol
right2left m s = passMag m R s

reflect :: Reflector -> Symbol -> Symbol
reflect r s = r ! s

left2right :: Magazine -> Symbol -> Symbol
left2right m s = passMag m L s

passEnigma :: Enigma -> Symbol -> Symbol
passEnigma e = left2right m . reflect re . right2left m
  where
    m  = magazine e
    re = reflector e

nextEnigmaState :: Enigma -> Enigma
nextEnigmaState e = adjustEnigmaState e (incStateNumber st)
  where
    st = mState $ magazine e

passSymbol :: Enigma -> Symbol -> (Symbol, Enigma)
passSymbol e s
  | not $ s >= 0 && s < rotorSize = error "symbol out of bounds"
  | otherwise = ( passEnigma e s
                , nextEnigmaState e)
  where
    m  = magazine e
    re = reflector e

encrypt :: Enigma -> [Symbol] -> ([Symbol], Enigma)
encrypt e
  = foldr aux ([],e)
  where
    aux s (acc, e) =
      let (s', e') = passSymbol e s
      in (s':acc, e')


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

nthFactoryReflector :: SerialNumber -> Reflector
nthFactoryReflector s =
  let ps = getNthPairCombination pins s
  in foldr (\p acc -> insert (swap p) (insert p acc)) M.empty ps
  where
    insert p = uncurry M.insert p
