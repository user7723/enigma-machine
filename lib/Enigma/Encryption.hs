{-# LANGUAGE RecordWildCards #-}

module Enigma.Encryption
  ( encrypt
  ) where

import qualified Data.Array.Unboxed as A

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Enigma.Aliases
import Enigma (Enigma(..), incEnigmaState)
import Enigma.Magazine  (Magazine(..))
import Enigma.Reflector (Reflector)

spin :: Offset -> Offset -> Pin -> Pin
spin o1 o2 p = o2 - o1 + p
{-# INLINE spin #-}

mapWithOffset :: Table -> Offset -> Offset -> Pin -> Pin
mapWithOffset r o1 o2 p = r A.! (spin o1 o2 p)
{-# INLINE mapWithOffset #-}

runThroughRotors
  :: (Table,Table,Table)
  -> (Offset,Offset,Offset)
  -> Pin
  -> Pin
runThroughRotors (r1,r2,r3) (o1,o2,o3)
  = spin o3 0
  . mapWithOffset r3 o2 o3
  . mapWithOffset r2 o1 o2
  . mapWithOffset r1 0  o1
{-# INLINE runThroughRotors #-}

right2left :: Magazine -> Pin -> Pin
right2left Magazine{..} p =
  (\(o1,o2,o3) (r1,r2,r3)
  -> runThroughRotors (r1,r2,r3) (o1,o2,o3) p) magO magR
{-# INLINE right2left #-}

reflect :: Reflector -> Pin -> Pin
reflect r s = r A.! s
{-# INLINE reflect #-}

left2right :: Magazine -> Pin -> Pin
left2right Magazine{..} p =
  (\(o3,o2,o1) (r3,r2,r1)
  -> runThroughRotors (r1,r2,r3) (o1,o2,o3) p) magO magL
{-# INLINE left2right #-}

passEnigma :: Enigma -> Pin -> Pin
passEnigma Enigma{..}
  = left2right magazine
  . reflect    reflector
  . right2left magazine
{-# INLINE passEnigma #-}

passPin :: Enigma -> Pin -> (Enigma, Pin)
passPin e p = (incEnigmaState e, passEnigma e p)
{-# INLINE passPin #-}

passPins :: Enigma -> ByteString -> (Enigma, ByteString)
passPins = B.mapAccumR passPin

encrypt :: Enigma -> ByteString -> ByteString
encrypt e = snd . passPins e
