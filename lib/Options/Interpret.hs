{-# LANGUAGE RecordWildCards #-}

module Options.Interpret
  ( interpretProgSpec
  , Program(..)
  , ProgramIO(..)
  ) where

import qualified Data.ByteString as B
import System.IO (stdin, stdout, openFile, IOMode(..))

import Options.Parse
  ( ProgSpec(..)
  , EnigmaSpec(..)
  , EnigmaSpecOpt(..)
  , EnigmaSpecFile(..)
  , IOSpec(..)
  , InputSpec(..)
  , OutputSpec(..)
  , Rots(..)
  , parserOpts
  , runArgs
  )
import Options.Interact (interactiveReadSpec)
import Enigma.Aliases (IHandle, OHandle)
import Enigma.Enigma (Enigma, initEnigma)
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
  { progInput  :: IHandle
  , progOutput :: OHandle
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
interpretEnigmaSpec (EnigmaSpecO EnigmaSpecOpt{..})
  = return $ initEnigma re mag stateNumber
  where
    Rots{..} = rotorNumbers
    mag = initMagazine rot1 rot2 rot3
    re  = nthFactoryReflector reflectorNumber

configFileReadSpec :: FilePath -> IO Enigma
configFileReadSpec = undefined
