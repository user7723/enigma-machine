{-# LANGUAGE RecordWildCards #-}

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
initEnigma r m s = Enigma r (setMagazineState m s)

incEnigmaState :: Enigma -> Enigma
incEnigmaState e@Enigma{..} =
  e { magazine = incMagazineState magazine }

{-# INLINE incEnigmaState #-}
