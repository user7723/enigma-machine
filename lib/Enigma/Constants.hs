module Enigma.Constants
  ( rotorSize
  , pins
  , maxReflectorNumber
  , maxRotorNumber
  , maxMagazineState
  , rotorsCount
  , alphabetBounds
  ) where

import Enigma.Aliases
import Combinatorics.Common
import Combinatorics.UPairsTree (combsOfDistinctUPairs)

-- N.B. The amount of characters must be even
alphaMax :: Pin
alphaMax = maxBound

alphaMin :: Pin
alphaMin = minBound

alphabetBounds :: (Pin, Pin)
alphabetBounds = (alphaMin, alphaMax)

-- the rotor size can be safely adjusted as needed
-- so we probably can make it big enough to encode the ascii range
-- or at least some alpha-numerics with space symbols
rotorSize :: Int
rotorSize = fromIntegral alphaMax + 1

rotorsCount :: Int
rotorsCount = 3

pins :: [Pin]
pins = [alphaMin .. alphaMax]

maxReflectorNumber :: SerialNumber
maxReflectorNumber = combsOfDistinctUPairs $ fromIntegral rotorSize

maxRotorNumber :: SerialNumber
maxRotorNumber = fac $ fromIntegral rotorSize

maxMagazineState :: StateNumber
maxMagazineState = rotorSize ^ rotorsCount
