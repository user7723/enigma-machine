{-# LANGUAGE RecordWildCards #-}

module Enigma.Encryption
  ( encrypt
  ) where

import qualified Data.Array.Unboxed as A


import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Enigma.Aliases
import Enigma.Constants (rotorSize, alphabetBounds)
import Enigma.Enigma    (Enigma(..), incEnigmaState)
import Enigma.Magazine  (Magazine(..), getRotors)
import Enigma.Reflector (Reflector)
import Enigma.Rotor     (Rotor(..), RotorSide(..), RotorSt(..), getRotorSide)

-- It's pretty safe to use it
-- [mapWithOffset o1 o2 p | o1 <- pins, o2 <- pins, p <- pins]
mapWithOffset :: Offset -> Offset -> Pin -> Pin
mapWithOffset o1 o2 p = (o2 - o1) + p

passRot :: RotorSide -> RotorSt -> (Offset, Pin) -> (Offset, Pin)
passRot lr rs (o1, p)
  = (\o2 -> (o2, getRotorSide lr rs A.! mapWithOffset o1 o2 p))
  $ rOffset rs

swapOrder :: RotorSide -> (a,a,a) -> (a,a,a)
swapOrder R t       = t
swapOrder L (a,b,c) = (c,b,a)

passMag :: Magazine -> RotorSide -> Pin -> Pin
passMag m lr p =
  (\(r1,r2,r3)
    -> uncurry (flip mapWithOffset 0)
    $ passRot lr r3
    $ passRot lr r2
    $ passRot lr r1 (0, p)
  ) $ swapOrder lr $ getRotors m

right2left :: Magazine -> Pin -> Pin
right2left m s = passMag m R s

reflect :: Reflector -> Pin -> Pin
reflect r s = r A.! s

left2right :: Magazine -> Pin -> Pin
left2right m s = passMag m L s

-- 'PassPin' takes input symbol by modulo of 'rotorSize'
-- so it becomes safe to use '!' operator, because rotors
-- enumerate their pins from 0 up to 'rotorSize' non-inclusive
passPin :: Enigma -> Pin -> (Enigma, Pin)
passPin e p =
  ( incEnigmaState e
  , passEnigma e p)

passEnigma :: Enigma -> Pin -> Pin
passEnigma Enigma{..}
  = left2right magazine
  . reflect    reflector
  . right2left magazine

-- mapAccumR ::
-- (Enigma -> Word8 -> (Enigma, Word8)) -> Enigma -> ByteString -> (Enigma, ByteString)
passPins :: Enigma -> ByteString -> (Enigma, ByteString)
passPins = B.mapAccumR passPin

encrypt :: Enigma -> ByteString -> ByteString
encrypt e = snd . passPins e
