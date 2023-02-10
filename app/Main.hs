{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T

import Control.Monad (unless)

import System.Environment (getArgs)
import System.IO (hClose, hIsClosed, stdout, stdin)

import Enigma.Encryption (encrypt)

import Options.Interpret (runArgs, exitFail)

main :: IO ()
main = do
  args <- getArgs
  mt <- runArgs args
  case mt of
    Nothing -> exitFail
    Just (enigma, inp, out) -> do
      txt <- T.hGetContents inp
      let etxt = encrypt enigma txt
      T.hPutStr out etxt
      inpc <- hIsClosed inp
      outc <- hIsClosed out
      unless (inpc || inp == stdin)  $ hClose inp
      unless (outc || out == stdout) $ hClose out
