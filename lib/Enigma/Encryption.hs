module Enigma.Encryption
  ( encrypt
  , decrypt
  ) where

import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as M

import Data.Maybe (catMaybes)

import qualified Data.Text as T
import Data.Text (Text)

import Enigma.Aliases
import Enigma.Constants (rotorSize, alphabet, alphaMap)
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

-- 'PassSymbol' takes input symbol by modulo of 'rotorSize'
-- so it becomes safe to use '!' operator, because rotors
-- enumerate their pins from 0 up to 'rotorSize' non-inclusive
passSymbol :: Enigma -> Symbol -> (Symbol, Enigma)
passSymbol e s =
  ( passEnigma e (s `mod` rotorSize)
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

encrypt :: Enigma -> Text -> Text
encrypt e = translateFrom . fst . passSymbols e . translateTo

decrypt :: Enigma -> Text -> Text
decrypt = encrypt

charToSymbol :: Char -> Maybe Symbol
charToSymbol c = M.lookup c cs
  where
    (_,cs) = alphaMap

symbolToChar :: Symbol -> Char
symbolToChar = (sc !) . (`mod` rotorSize)
  where
    (sc,_) = alphaMap

translateFrom :: [Symbol] -> Text
translateFrom = T.pack . map symbolToChar

translateTo :: Text -> [Symbol]
translateTo = catMaybes . map charToSymbol . T.unpack
