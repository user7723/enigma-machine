module Enigma.Encryption
  ( encrypt
  , decrypt
  ) where

import Data.Map (Map, (!))

import Enigma.Aliases
import Enigma.Constants (rotorSize)
import Enigma.Enigma    (Enigma(..), nextEnigmaState)
import Enigma.Magazine  (Magazine(..), getRotors)
import Enigma.Reflector (Reflector)
import Enigma.Rotor     (Rotor(..), RotorSide(..), RotorSt(..))

-- It's pretty safe to use it
-- [mapWithOffset o1 o2 p | o1 <- pins, o2 <- pins, p <- pins]
mapWithOffset :: Offset -> Offset -> Pin -> Pin
mapWithOffset o1 o2 p = fromIntegral $
  (rsi + (o2i - o1i) + pi) `mod` rsi
  where
    rsi = fromIntegral rotorSize :: Integer
    pi  = fromIntegral p         :: Integer
    o1i = fromIntegral o1        :: Integer
    o2i = fromIntegral o2        :: Integer

passRot :: RotorSt -> RotorSide -> Offset -> Symbol -> (Symbol, Offset)
passRot rs lr o1 s =
  let p = mapWithOffset o1 o2 s
  in  (r ! p, o2)
  where
    (o2,r) = case lr of
      L -> (rOffset rs, rLeft $ rotor rs)
      R -> (rOffset rs, rRigth $ rotor rs)

passMag :: Magazine -> RotorSide -> Symbol -> Symbol
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

right2left :: Magazine -> Symbol -> Symbol
right2left m s = passMag m R s

reflect :: Reflector -> Symbol -> Symbol
reflect r s = r ! s

left2right :: Magazine -> Symbol -> Symbol
left2right m s = passMag m L s

passSymbol :: Enigma -> Symbol -> (Symbol, Enigma)
passSymbol e s
  | not $ s >= 0 && s < rotorSize = error "symbol out of bounds"
  | otherwise = ( passEnigma e s
                , nextEnigmaState e)
  where
    m  = magazine e
    re = reflector e

passEnigma :: Enigma -> Symbol -> Symbol
passEnigma e = left2right m . reflect re . right2left m
  where
    m  = magazine e
    re = reflector e

passSymbols :: Enigma -> [Symbol] -> ([Symbol], Enigma)
passSymbols e
  = foldr aux ([],e)
  where
    aux s (acc, e) =
      let (s', e') = passSymbol e s
      in (s':acc, e')

encrypt :: Enigma -> [Symbol] -> [Symbol]
encrypt = (fst .) . passSymbols

decrypt :: Enigma -> [Symbol] -> [Symbol]
decrypt = encrypt
