module Enigma.Encryption
  ( encrypt
  ) where

import qualified Data.Array.Unboxed as A

import qualified Data.Text as T
import Data.Text (Text)

import Enigma.Aliases
import Enigma.Constants (rotorSize, alphabetBounds)
import Enigma.Enigma    (Enigma(..), incEnigmaState)
import Enigma.Magazine  (Magazine(..), getRotors)
import Enigma.Reflector (Reflector)
import Enigma.Rotor     (Rotor(..), RotorSide(..), RotorSt(..))

-- It's pretty safe to use it
-- [mapWithOffset o1 o2 p | o1 <- pins, o2 <- pins, p <- pins]
mapWithOffset :: Offset -> Offset -> Pin -> Pin
mapWithOffset o1 o2 p = (rs + (o2 - o1) + p) `mod` rs
  where rs = rotorSize

passRot :: RotorSt -> RotorSide -> Offset -> Pin -> (Pin, Offset)
passRot rs lr o1 s =
  let p = mapWithOffset o1 o2 s
  in  (r A.! p, o2)
  where
    (o2,r) = case lr of
      L -> (rOffset rs, rLeft $ rotor rs)
      R -> (rOffset rs, rRigth $ rotor rs)

passMag :: Magazine -> RotorSide -> Pin -> Pin
passMag m lr s =
  let (s1,o1) = passRot r1 lr 0 s
      (s2,o2) = passRot r2 lr o1 s1
      (s3,o3) = passRot r3 lr o2 s2
  in mapWithOffset o3 0 s3
  where
    rs@(r1', r2', r3') = getRotors m
    (r1, r2, r3) = case lr of
      L -> (r3', r2', r1')
      _ -> rs

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
passPin e s =
  ( incEnigmaState e
  , passEnigma e (s `mod` rotorSize))

passEnigma :: Enigma -> Pin -> Pin
passEnigma e = left2right m . reflect re . right2left m
  where
    m  = magazine e
    re = reflector e

-- mapAccumR ::
-- (Enigma -> Char -> (Enigma, Char)) -> Enigma -> Text -> (Enigma, Text)
passPins :: Enigma -> Text -> (Enigma, Text)
passPins = T.mapAccumR aux
  where
    aux :: Enigma -> Char -> (Enigma, Char)
    aux e c
      | A.inRange alphabetBounds c = fmap toEnum $ passPin e $ fromEnum c
      | otherwise = (e, c)

encrypt :: Enigma -> Text -> Text
encrypt e = snd . passPins e
