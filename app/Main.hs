{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Text.IO as T

import Control.Monad (unless)
import Control.Applicative (liftA2)

import System.Environment (getArgs)
import System.IO (IOMode(..), openFile, hClose, hIsClosed, stdout, stdin)
import System.Exit (ExitCode(..), exitWith)

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
  args <- getArgs
  (inp, out) <-
    case args of
      []      -> pure (stdin, stdout)
      (x:[])  -> liftA2 (,) (openFile x ReadMode) (pure stdout)
      (x:y:_) -> liftA2 (,) (openFile x ReadMode) (openFile y WriteMode)
  txt <- T.hGetContents inp
  let etxt = encrypt enigma txt
  T.hPutStr out etxt
  inpc <- hIsClosed inp
  outc <- hIsClosed out

  unless (inpc || inp == stdin)  $ hClose inp
  unless (outc || out == stdout) $ hClose out
