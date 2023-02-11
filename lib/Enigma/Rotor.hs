module Enigma.Rotor
  ( Rotor
  , nthFactoryRotor
  ) where

import Combinatorics.PermutationTree (nthPermutation)
import Enigma.Constants (alphabetBounds, pins)
import Enigma.Aliases (Table, SerialNumber)

import qualified Data.Array.Unboxed as A

-- There is two maps because we need routes to go in
-- opposite direction after reflection
type Rotor = (Table, Table)

-- the right and left sides of the Rotor are ordered numerically
-- and correspond to each other in geometric sense
-- it's their commutation that is mixed
nthFactoryRotor :: SerialNumber -> Rotor
nthFactoryRotor s = (,)
  (A.array alphabetBounds $ zip pins ps)
  (A.array alphabetBounds $ zip ps pins)
  where
    ps = nthPermutation s pins
