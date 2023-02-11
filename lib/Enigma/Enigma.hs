module Enigma.Enigma
  ( Enigma(..)
  , initEnigma
  , incEnigmaState
  ) where

import Enigma.Aliases
import Enigma.Magazine  (Magazine, incMagazineState, setMagazineState)
import Enigma.Reflector (Reflector)

data Enigma = Enigma
  { reflector :: Reflector
  , magazine  :: Magazine
  } deriving Show

initEnigma :: Reflector -> Magazine -> StateNumber -> Enigma
initEnigma r m sn = Enigma r (setMagazineState sn m)

incEnigmaState :: Enigma -> Enigma
incEnigmaState e =
  let m = magazine e
  in e{magazine = incMagazineState m}
