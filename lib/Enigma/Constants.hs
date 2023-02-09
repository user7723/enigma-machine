module Enigma.Constants
  ( rotorSize
  , pins
  , alphabet
  , maxReflectorNumber
  , maxRotorNumber
  , maxMagazineState
  , rotorsCount
  , alphaMap
  ) where

import Numeric.Natural

import Enigma.Aliases
import Combinatorics.Common
import Combinatorics.UPairsTree (combsOfDistinctUPairs)

import Data.Map (Map)
import qualified Data.Map as M

-- N.B. The amount of characters must be even
-- The printable range of ascii except for 127th char which is \delete
-- the newline char is insted of it
alphabet :: [Char]
alphabet = map toEnum [32 .. 126] ++ ['\n']

alphaMap :: (Map Symbol Char, Map Char Symbol)
alphaMap =
  ( M.fromList $ zip [0..] alphabet
  , M.fromList $ zip alphabet [0..])

-- the rotor size can be safely adjusted as needed
-- so we probably can make it big enough to encode the ascii range
-- or at least some alpha-numerics with space symbols
rotorSize :: Pin
rotorSize = fromIntegral $ length alphabet

rotorsCount :: Natural
rotorsCount = 3

pins :: [Pin]
pins = [0 .. rotorSize - 1]

maxReflectorNumber :: SerialNumber
maxReflectorNumber = combsOfDistinctUPairs rotorSize

maxRotorNumber :: SerialNumber
maxRotorNumber = fac rotorSize

maxMagazineState :: StateNumber
maxMagazineState = rotorSize ^ rotorsCount
