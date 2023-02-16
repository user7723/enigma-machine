module Enigma.Constants
  ( rotorSize
  , pins
  , maxReflectorNumber
  , maxRotorNumber
  , maxMagazineState
  , rotorsCount
  , alphabetBounds
  , boundsInfo
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

boundsInfo :: String
boundsInfo = mconcat
  [ "1. Alphabet in use:\n", show alphabetBounds, "\n\n"
  , "2. Alphabet size (n):\n", show rotorSize, "\n\n"
  , "3. Max rotor serial number (n!):\n", show maxRotorNumber , "\n\n"
  , "4. Max reflector serial number (n!/(n/2)!*2^(n/2)):\n", show maxReflectorNumber , "\n\n"
  , "5. Amount of rotors (m):\n", show rotorsCount, "\n\n"
  , "6. Max state number of enigma (n^m):\n", show maxMagazineState, "\n"
  ]
