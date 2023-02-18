module Enigma.Reflector
  ( Reflector
  , SerialNumber
  , nthFactoryReflector
  ) where

import Data.Tuple (swap)

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as A


import Enigma.Combinatorics.UPairsTree (getNthPairCombination)
import Enigma.Constants (pins)
import Enigma.Aliases (Pin, SerialNumber)

type Reflector = UArray Pin Pin

nthFactoryReflector :: SerialNumber -> Reflector
nthFactoryReflector s =
  let ps = concatMap (\p -> [p, swap p]) $ getNthPairCombination pins s
      b  = (0, fromIntegral $ length ps - 1)
  in A.array b ps
