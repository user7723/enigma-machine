{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Text.IO as TIO

import Enigma.Constants
import Enigma.Enigma     (initEnigma)
import Enigma.Reflector  (nthFactoryReflector)
import Enigma.Magazine   (initMagazine)
import Enigma.Encryption (encrypt, decrypt)

enigma =
  initEnigma
    (nthFactoryReflector 0)
    (initMagazine 12933993 912391293 2939232)
    83838

info :: Text
info = mconcat
  [ "max rotor \"serial number\":         ", T.pack $ show maxRotorNumber , "\n"
  , "max reflector \"serial number\":     ", T.pack $ show maxReflectorNumber , "\n"
  , "amount of possible states of enigma: ", T.pack $ show maxMagazineState, "\n"
  ]

main :: IO ()
main = do
  TIO.putStrLn info
  let msg = "Hello, world!"
      emsg = encrypt enigma msg
  TIO.putStrLn msg
  print emsg
  let msg' = decrypt enigma emsg
  TIO.putStrLn msg'
