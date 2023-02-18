{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString as B

import Control.Monad (unless)

import System.IO (hClose, hIsClosed, stdout, stdin)

import Enigma.Encryption (encrypt)
import Enigma.Options.Interpret
  ( Program(..)
  , ProgramIO(..)
  , interpretProgSpec
  )
import Enigma.Options.Parse (runArgs)

main :: IO ()
main = do
  pspec <- runArgs
  Program{..} <- interpretProgSpec pspec
  let ProgramIO{..} = progIO

  txt <- B.hGetContents progInput
  let etxt = encrypt progEnigma txt
  B.hPut progOutput etxt

  inpc <- hIsClosed progInput
  unless (inpc || progInput == stdin)   $ hClose progInput

  outc <- hIsClosed progOutput
  unless (outc || progOutput == stdout) $ hClose progOutput
