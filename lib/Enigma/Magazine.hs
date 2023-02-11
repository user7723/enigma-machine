{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Enigma.Magazine
  ( Magazine(..)
  , initMagazine
  , setMagazineState
  , getRotors
  , incMagazineState
  ) where

import Enigma.Aliases (StateNumber, SerialNumber)
import Enigma.Rotor (RotorSt, rotateRotor, setRotorState, initNthRotor, splitState)

data Magazine = Magazine
  { getR3  :: RotorSt
  , getR2  :: RotorSt
  , getR1  :: RotorSt
  } deriving Show

initMagazine :: SerialNumber -> SerialNumber -> SerialNumber -> Magazine
initMagazine s1 s2 s3 = Magazine
  { getR1 = initNthRotor s1
  , getR2 = initNthRotor s2
  , getR3 = initNthRotor s3
  }

setMagazineState :: StateNumber -> Magazine -> Magazine
setMagazineState s Magazine{..} =
  let (x1,x2,x3) = splitState s
  in Magazine
    { getR1 = setRotorState x1 getR1
    , getR2 = setRotorState x2 getR2
    , getR3 = setRotorState x3 getR3
    }

zeroStateMagazine :: Magazine -> Magazine
zeroStateMagazine Magazine{..} =
  let z = setRotorState 0
  in Magazine
    { getR1 = z getR1
    , getR2 = z getR2
    , getR3 = z getR3
    }

incMagazineState :: Magazine -> Magazine
incMagazineState m@Magazine{..} =
  let
    (o1, r1') = rotateRotor getR1
    (o2, r2') = rotateRotor getR2
    (o3, r3') = rotateRotor getR3
  in if | not o1    -> m{ getR1 = r1' }
        | not o2    -> m{ getR1 = r1', getR2 = r2' }
        | not o3    -> m{ getR1 = r1', getR2 = r2', getR3 = r3' }
        | otherwise -> zeroStateMagazine m

getRotors :: Magazine -> (RotorSt, RotorSt, RotorSt)
getRotors m = (getR1 m, getR2 m, getR3 m)
