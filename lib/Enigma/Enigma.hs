module Enigma.Enigma
  ( Enigma(..)
  , enigmaSample
  , initEnigma
  , adjustEnigmaState
  , nextEnigmaState
  ) where

import Enigma.Aliases
import Enigma.Magazine  (Magazine(..) ,initMagazine, setMagazineState)
import Enigma.Reflector (Reflector, nthFactoryReflector)
import Enigma.Rotor     (incStateNumber)

data Enigma = Enigma
  { reflector :: Reflector
  , magazine  :: Magazine
  } deriving Show

enigmaSample =
  initEnigma
    (nthFactoryReflector 0)
    (initMagazine 12933993 912391293 2939232)
    0

initEnigma :: Reflector -> Magazine -> StateNumber -> Enigma
initEnigma r m sn = Enigma r (setMagazineState sn m)

adjustEnigmaState :: Enigma -> StateNumber -> Enigma
adjustEnigmaState e sn = e { magazine = (setMagazineState sn m) }
  where m = magazine e

nextEnigmaState :: Enigma -> Enigma
nextEnigmaState e = adjustEnigmaState e (incStateNumber st)
  where
    st = mState $ magazine e
