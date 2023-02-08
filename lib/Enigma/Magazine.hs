module Enigma.Magazine
  ( Magazine(..)
  , initMagazine
  , setMagazineState
  , getRotors
  ) where

import Enigma.Aliases (StateNumber, SerialNumber)
import Enigma.Rotor (RotorSt, setRotorState, initNthRotor, splitState)

data Magazine = Magazine
  { getR3  :: RotorSt
  , getR2  :: RotorSt
  , getR1  :: RotorSt
  , mState :: StateNumber
  } deriving Show

initMagazine :: SerialNumber -> SerialNumber -> SerialNumber -> Magazine
initMagazine s1 s2 s3 = Magazine
  { getR1 = initNthRotor s1
  , getR2 = initNthRotor s2
  , getR3 = initNthRotor s3
  , mState = 0
  }

setMagazineState :: StateNumber -> Magazine -> Magazine
setMagazineState s m =
  let (x1,x2,x3) = splitState s
      (r1, r2, r3) = getRotors m
  in Magazine
    { getR1 = setRotorState x1 r1
    , getR2 = setRotorState x2 r2
    , getR3 = setRotorState x3 r3
    , mState = s
    }

getRotors :: Magazine -> (RotorSt, RotorSt, RotorSt)
getRotors m = (getR1 m, getR2 m, getR3 m)
