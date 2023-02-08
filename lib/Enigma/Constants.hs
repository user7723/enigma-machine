module Enigma.Constants
  ( rotorSize
  , pins
  , alphabet
  , fstChar
  , maxReflectorNumber
  , maxRotorNumber
  , maxMagazineState
  , rotorsCount
  ) where

import Numeric.Natural

import Enigma.Aliases
import Combinatorics.Common
import Combinatorics.UPairsTree (combsOfDistinctUPairs)

-- N.B. The amount of characters must be even
alphabet :: [Char]
alphabet = map toEnum [32 .. 127]

fstChar :: Char
fstChar = head alphabet

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
