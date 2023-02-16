{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck

import Enigma.Constants
import Enigma.Aliases
import Enigma.Reflector (Reflector, nthFactoryReflector)
import Enigma.Enigma (Enigma, initEnigma)
import Enigma.Magazine (Magazine, initMagazine)
import Enigma.Encryption (encrypt)

import Data.Word (Word8)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

gRo :: Gen Integer
gRo = choose (0, maxRotorNumber - 1)

gRe :: Gen Integer
gRe = choose (0, maxReflectorNumber - 1)

gSt :: Gen Int
gSt = choose (0, maxMagazineState - 1)

instance Arbitrary Magazine where
  arbitrary = initMagazine <$> gRo <*> gRo <*> gRo

instance Arbitrary Enigma where
  arbitrary = initEnigma <$> (nthFactoryReflector <$> gRe) <*> arbitrary <*> gSt

alphChar :: Gen Word8
alphChar = choose alphabetBounds

instance Arbitrary ByteString where
  arbitrary
    =  B.pack
   <$> (resize (fromIntegral maxMagazineState + 1000)
    $  listOf alphChar)

prop_pack =
  forAll alphChar $ \c -> (B.unpack $ B.pack [c]) == [c]

prop_inverse =
  forAll ((,)
        <$> (arbitrary :: Gen Enigma)
        <*> (arbitrary :: Gen ByteString)) $ \(e,t)->
  encrypt e (encrypt e t) == t

main :: IO ()
main = do
  quickCheck prop_inverse
