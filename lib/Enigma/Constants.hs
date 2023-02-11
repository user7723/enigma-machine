module Enigma.Constants
  ( rotorSize
  , pins
  , alphabet
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
alphaMax :: Char
alphaMax = toEnum 255

alphaMin :: Char
alphaMin = minBound

alphabet :: [Char]
alphabet = [alphaMin .. alphaMax]

alphabetBounds :: (Char, Char)
alphabetBounds = (alphaMin, alphaMax)

-- the rotor size can be safely adjusted as needed
-- so we probably can make it big enough to encode the ascii range
-- or at least some alpha-numerics with space symbols
rotorSize :: Pin
rotorSize = length alphabet

rotorsCount :: Int
rotorsCount = 3

pins :: [Pin]
pins = [0 .. rotorSize - 1]

maxReflectorNumber :: SerialNumber
maxReflectorNumber = combsOfDistinctUPairs $ fromIntegral rotorSize

maxRotorNumber :: SerialNumber
maxRotorNumber = fac $ fromIntegral rotorSize

maxMagazineState :: StateNumber
maxMagazineState = rotorSize ^ rotorsCount
