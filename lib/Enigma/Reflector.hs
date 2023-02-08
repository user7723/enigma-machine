module Enigma.Reflector
  ( Reflector
  , SerialNumber
  , nthFactoryReflector
  ) where

import Numeric.Natural
import Data.Tuple (swap)

import Data.Map (Map)
import qualified Data.Map as M

import Combinatorics.UPairsTree (getNthPairCombination)
import Enigma.Constants (pins)
import Enigma.Aliases (Pin)

type SerialNumber = Natural
type Reflector    = Map Pin Pin

nthFactoryReflector :: SerialNumber -> Reflector
nthFactoryReflector s =
  let ps = getNthPairCombination pins s
  in foldr (\p acc -> insert (swap p) (insert p acc)) M.empty ps
  where
    insert p = uncurry M.insert p
