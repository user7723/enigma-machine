module Enigma.Aliases
  ( Offset
  , StateNumber
  , SerialNumber
  , Pin
  , Overflow
  , Table
  ) where

import Data.Word (Word8)
import Data.Array.Unboxed (UArray)

type Offset       = Word8
type StateNumber  = Int
type SerialNumber = Integer
type Pin          = Word8
type Overflow     = Word8
type Table        = UArray Pin Pin
