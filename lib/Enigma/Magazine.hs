{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Enigma.Magazine
  ( Magazine(..)
  , initMagazine
  , incMagazineState
  , setMagazineState
  ) where

import qualified Control.Monad.State.Strict as S

import Data.Tuple (swap)
import Enigma.Constants (rotorSize, rotorsCount)
import Enigma.Aliases (StateNumber, SerialNumber, Overflow, Offset, Table)
import Enigma.Rotor (nthFactoryRotor)

data Magazine = Magazine
  { magL :: (Table, Table, Table)
  , magR :: (Table, Table, Table)
  , magO :: (Offset, Offset, Offset)
  } deriving Show

initMagazine :: SerialNumber -> SerialNumber -> SerialNumber -> Magazine
initMagazine s1 s2 s3 = Magazine
  { magL = (l1,l2,l3)
  , magR = (r1,r2,r3)
  , magO = (0,0,0)
  }
  where
    (l1, r1) = nthFactoryRotor s1
    (l2, r2) = nthFactoryRotor s2
    (l3, r3) = nthFactoryRotor s3

setMagazineState :: Magazine -> StateNumber -> Magazine
setMagazineState m s = m { magO = splitState s }

splitState :: StateNumber -> (Offset,Offset,Offset)
splitState s = flip S.evalState s0 $ do
  x1 <- st
  x2 <- st
  x3 <- st
  return (x1, x2, x3)
  where
    s0 = s `mod` (rotorSize^rotorsCount)
    st = fmap fromIntegral $ S.state d
    d = swap . flip divMod rotorSize

-- if the result of increment is less than the previous value
-- then there is an overflow
ovf :: Offset -> Offset -> Offset
ovf o' o | o' < o = 1
         | otherwise = 0
{-# INLINE ovf #-}

rot :: Offset -> S.State Overflow Offset
rot o = S.state $ (\o' -> (o', ovf o' o)) . (o +)
{-# INLINE rot #-}

rotT :: (Offset,Offset,Offset) -> (Offset,Offset,Offset)
rotT (o1,o2,o3) = flip S.evalState 1 $ do
  o1' <- rot o1
  o2' <- rot o2
  o3' <- rot o3
  return (o1', o2', o3')
{-# INLINE rotT #-}

incMagazineState :: Magazine -> Magazine
incMagazineState m@Magazine{..} = m { magO = rotT magO }
{-# INLINE incMagazineState #-}
