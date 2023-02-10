module Enigma.Aliases
  ( Offset
  , StateNumber
  , SerialNumber
  , Symbol
  , Pin
  , Overflow
  , IFilePath
  , OFilePath
  , IHandle
  , OHandle
  ) where

import Numeric.Natural
import System.IO (Handle, FilePath)

type Offset       = Natural
type StateNumber  = Natural
type SerialNumber = Natural
type Symbol       = Natural
type Pin          = Natural
type Overflow     = Bool

type IFilePath = FilePath
type OFilePath = FilePath
type IHandle   = Handle
type OHandle   = Handle
