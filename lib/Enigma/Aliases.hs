module Enigma.Aliases
  ( Offset
  , StateNumber
  , SerialNumber
  , Pin
  , Overflow
  , IFilePath
  , OFilePath
  , IHandle
  , OHandle
  ) where

import Numeric.Natural
import System.IO (Handle, FilePath)

type Offset       = Int
type StateNumber  = Int
type SerialNumber = Natural
type Pin          = Int
type Overflow     = Bool

type IFilePath = FilePath
type OFilePath = FilePath
type IHandle   = Handle
type OHandle   = Handle
