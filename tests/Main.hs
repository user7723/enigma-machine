{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T

import Numeric.Natural
import Enigma.Constants
import Enigma.Aliases
import Enigma.Reflector (Reflector, nthFactoryReflector)
import Enigma.Enigma (Enigma, initEnigma)
import Enigma.Magazine (Magazine, initMagazine)
import Enigma.Encryption (encrypt)


instance Arbitrary Natural where
  arbitrary = (fromIntegral . abs) <$> (arbitrary :: Gen Integer)

gRo :: Gen Natural
gRo = fromIntegral <$> (choose (0, fromIntegral maxRotorNumber - 1) :: Gen Integer)

gRe :: Gen Natural
gRe = fromIntegral <$> (choose (0, fromIntegral maxReflectorNumber - 1) :: Gen Integer)

gSt :: Gen Natural
gSt = fromIntegral <$> (choose (0, fromIntegral maxMagazineState - 1) :: Gen Integer)

instance Arbitrary Magazine where
  arbitrary = initMagazine <$> gRo <*> gRo <*> gRo

instance Arbitrary Enigma where
  arbitrary = initEnigma <$> (nthFactoryReflector <$> gRe) <*> arbitrary <*> gSt

alphChar :: Gen Char
alphChar = elements alphabet

instance Arbitrary Text where
  arbitrary = resize (fromIntegral maxMagazineState + 100000) $ T.pack <$> listOf alphChar

prop_inverse =
  forAll ((,)
        <$> (arbitrary :: Gen Enigma)
        <*> (arbitrary :: Gen Text)) $ \(e,t)->
  encrypt e (encrypt e t) == t

main :: IO ()
main = do
  quickCheck prop_inverse

