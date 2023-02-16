{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString as B

import Control.Monad (unless)

import System.Environment (getArgs)
import System.IO (hClose, hIsClosed, stdout, stdin)

import Enigma.Encryption (encrypt)
import Options.Interpret
  ( Program(..)
  , ProgramIO(..)
  , interpretProgSpec
  )
import Options.Parse (runArgs)

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
