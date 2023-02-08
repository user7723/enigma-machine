module Enigma.Constants
  ( rotorSize
  , pins
  ) where

import Enigma.Aliases

rotorSize :: Pin
rotorSize = 26

pins :: [Pin]
pins = [0 .. rotorSize - 1]
