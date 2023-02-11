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
  , getRotorSide
  ) where

import Combinatorics.PermutationTree (nthPermutation)
import Enigma.Constants (alphabetBounds, pins, rotorSize, rotorsCount)
import Enigma.Aliases (Table, SerialNumber, Pin, Offset, StateNumber, Overflow)

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as A

data RotorSide = L | R

-- There is two maps because we need routes to go in
-- opposite direction after reflection
data Rotor = Rotor
  { rLeft  :: UArray Pin Pin
  , rRigth :: UArray Pin Pin
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
  (A.array alphabetBounds $ zip pins ps)
  (A.array alphabetBounds $ zip ps pins)
  where
    ps = nthPermutation s pins

initNthRotor :: SerialNumber -> RotorSt
initNthRotor s = RotorSt 0 (nthFactoryRotor s)

rotateRotor :: RotorSt -> (Overflow, RotorSt)
rotateRotor (RotorSt o r) =
  let o' = o + 1
  in (o' < o, RotorSt o' r)

setRotorState :: Offset -> RotorSt -> RotorSt
setRotorState o (RotorSt _ r) = RotorSt o r

incStateNumber :: StateNumber -> StateNumber
incStateNumber s = (s + 1) `mod` rotorSize^rotorsCount

splitState :: StateNumber -> (Offset,Offset,Offset)
splitState s =
  let s0 = s `mod` (rotorSize^rotorsCount)
      (s1,x1) = s0 `divMod` rotorSize
      (s2,x2) = s1 `divMod` rotorSize
      (_ ,x3) = s2 `divMod` rotorSize
  in (fromIntegral x1, fromIntegral x2, fromIntegral x3)

getRotorSide :: RotorSide -> RotorSt -> Table
getRotorSide L = rLeft . rotor
getRotorSide R = rRigth . rotor
