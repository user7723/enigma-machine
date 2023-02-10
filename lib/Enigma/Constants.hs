module Enigma.Constants
  ( rotorSize
  , pins
  , alphabet
  , maxReflectorNumber
  , maxRotorNumber
  , maxMagazineState
  , rotorsCount
  , alphaMap
  , alphabetBounds
  ) where

import Enigma.Aliases
import Combinatorics.Common
import Combinatorics.UPairsTree (combsOfDistinctUPairs)

-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as M
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as A

-- N.B. The amount of characters must be even
-- The printable range of ascii except for 127th char which is \delete
-- the newline char is insted of it
-- alphabet :: [Char]
-- alphabet = map toEnum [32 .. 126] ++ ['\n']

alphaMax :: Char
alphaMax = toEnum 255

alphaMin :: Char
alphaMin = minBound

alphabet :: [Char]
alphabet = [alphaMin .. alphaMax]

alphabetBounds :: (Char, Char)
alphabetBounds = (alphaMin, alphaMax)

alphaMap :: (UArray Pin Char, UArray Char Pin)
alphaMap =
  ( A.array (0, rotorSize - 1) $ zip [0..] alphabet
  , A.array (minBound, maxBound) $ zip alphabet [0..])

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
