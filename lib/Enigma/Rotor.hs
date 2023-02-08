module Enigma.Rotor
  ( Rotor(..)
  , RotorSt(..)
  , RotorSide(..)
  , nthFactoryRotor
  , initNthRotor
  , rotateRotor
  , setRotorState
  , incStateNumber
  , splitState
  ) where

import Combinatorics.PermutationTree (nthPermutation)
import Enigma.Constants (pins, rotorSize)
import Enigma.Aliases (SerialNumber, Pin, Offset, StateNumber, Overflow)

import Data.Map (Map, (!))
import qualified Data.Map as M

data RotorSide = L | R

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

rotateRotor :: RotorSt -> (Overflow, RotorSt)
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