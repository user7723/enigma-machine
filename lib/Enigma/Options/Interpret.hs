{-# LANGUAGE RecordWildCards #-}

module Enigma.Options.Interpret
  ( interpretProgSpec
  , Program(..)
  , ProgramIO(..)
  ) where

import System.IO (Handle, stdin, stdout, openFile, IOMode(..))

import Enigma.Options.Parse
  ( ProgSpec(..)
  , EnigmaSpec(..)
  , EnigmaSpecOpt(..)
  , IOSpec(..)
  , Rots(..)
  )
import Enigma.Options.ConfigFile (readConfig, generateConfig)
import Enigma.Options.Interact (interactiveReadSpec)
import Enigma (Enigma, initEnigma)
import Enigma.Magazine (initMagazine)
import Enigma.Reflector (nthFactoryReflector)

data Program = Program
  { progEnigma :: Enigma
  , progIO     :: ProgramIO
  } deriving Show

interpretProgSpec :: ProgSpec -> IO Program
interpretProgSpec ProgSpec{..} = do
  e  <- interpretEnigmaSpec enigmaSpec
  io <- interpretIOSpec ioSpec
  return $ Program
    { progEnigma = e
    , progIO     = io
    }

data ProgramIO = ProgramIO
  { progInput  :: Handle
  , progOutput :: Handle
  } deriving Show

interpretIOSpec :: IOSpec -> IO ProgramIO
interpretIOSpec IOSpec{..} = do
  i <- maybe (pure stdin ) (flip openFile ReadMode ) inputSpec
  o <- maybe (pure stdout) (flip openFile WriteMode) outputSpec
  return ProgramIO
    { progInput  = i
    , progOutput = o
    }

interpretEnigmaSpec :: EnigmaSpec -> IO Enigma
interpretEnigmaSpec EnigmaSpecI
  = interactiveReadSpec
interpretEnigmaSpec (EnigmaSpecF specFilePath)
  = configFileReadSpec specFilePath
interpretEnigmaSpec (EnigmaSpecA specFilePath)
  = do generateConfig specFilePath
       interpretEnigmaSpec $ EnigmaSpecF specFilePath
interpretEnigmaSpec (EnigmaSpecO EnigmaSpecOpt{..})
  = return $ initEnigma re mag stateNumber
  where
    Rots{..} = rotorNumbers
    mag = initMagazine rot1 rot2 rot3
    re  = nthFactoryReflector reflectorNumber

configFileReadSpec :: FilePath -> IO Enigma
configFileReadSpec c = readConfig c >>= interpretEnigmaSpec
