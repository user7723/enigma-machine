module Enigma.Aliases
  ( Offset
  , StateNumber
  , SerialNumber
  , Pin
  , Overflow
  , Table
  , IFilePath
  , OFilePath
  , IHandle
  , OHandle
  ) where

import System.IO (Handle, FilePath)
import Data.Word (Word8)
import Data.Array.Unboxed (UArray)

type Offset       = Word8
type StateNumber  = Int
type SerialNumber = Integer
type Pin          = Word8
type Overflow     = Bool
type Table        = UArray Pin Pin

type IFilePath = FilePath
type OFilePath = FilePath
type IHandle   = Handle
type OHandle   = Handle
