module Options.Parse
  ( ProgSpec(..)
  , EnigmaSpec(..)
  , EnigmaSpecOpt(..)
  , EnigmaSpecFile(..)
  , Rots(..)
  , IOSpec(..)
  , InputSpec(..)
  , OutputSpec(..)
  , parserOpts
  , runArgs
  ) where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Text.RawString.QQ
 
import Options.Applicative
import Enigma.Constants (boundsInfo)
import Enigma.Aliases

runArgs :: IO ProgSpec
runArgs = execParser parserOpts

data ProgSpec = ProgSpec
  { enigmaSpec :: EnigmaSpec
  , ioSpec     :: IOSpec
  } deriving Show

parserOpts :: ParserInfo ProgSpec
parserOpts =
  info (progOptsp <**> helper)
       (  progDesc desc
       <> header hdr
       <> briefDesc )
  where
    hdr = "enigma - the Enigma Machine Model"
    desc = "Enigma is a command line program that (relatively inefficiently) encrypts your data either from a file or from `<stdin>`, and writes its output to a specified file or to `<stdout>`. It does not abide by the constraint of 26 symbols from the original hardware implementation, and currently it maps not the characters, but the bytes that represent them, so it's capable of encrypting text constituted with any set of characters."
    progOptsp = parserBoundsInfo <*> parserProgSpec

parserProgSpec :: Parser ProgSpec
parserProgSpec
   =  ProgSpec
  <$> parserEnigmaSpec
  <*> parserIOSpec

boundsInfoHelp :: String
boundsInfoHelp = "print max bounds of the enigma machine parameters, and other additional information"

-- prints bounds-info and fails
parserBoundsInfo :: Parser (ProgSpec -> ProgSpec)
parserBoundsInfo
  =  infoOption boundsInfo
  (  long "bounds-info"
  <> short 'b'
  <> help boundsInfoHelp
  )

data EnigmaSpec
  = EnigmaSpecO EnigmaSpecOpt
  | EnigmaSpecF EnigmaSpecFile
  | EnigmaSpecI
  deriving Show

parserEnigmaSpec :: Parser EnigmaSpec
parserEnigmaSpec = parserEnigmaSpecO <|> parserEnigmaSpecF <|> parserEnigmaSpecI

parserEnigmaSpecO :: Parser EnigmaSpec
parserEnigmaSpecO = EnigmaSpecO <$> parserEnigmaSpecOpt

parserEnigmaSpecF :: Parser EnigmaSpec
parserEnigmaSpecF = EnigmaSpecF <$> parserEnigmaSpecFile

parserEnigmaSpecI :: Parser EnigmaSpec
parserEnigmaSpecI = pure EnigmaSpecI

data EnigmaSpecOpt = EnigmaSpecOpt
  { rotorNumbers    :: Rots
  , reflectorNumber :: SerialNumber
  , stateNumber     :: StateNumber
  } deriving Show

parserEnigmaSpecOpt :: Parser EnigmaSpecOpt
parserEnigmaSpecOpt
   =  EnigmaSpecOpt
  <$> parserRots
  <*> parserReflector
  <*> parserStateNumber

readIntegral :: Integral a => ReadM a
readIntegral = eitherReader (fmap fst . T.decimal . T.pack)

parserStateNumber :: Parser StateNumber
parserStateNumber
  = option readIntegral
      (  long "init-state"
      <> short 's'
      <> metavar "<StateNumber>"
      <> help "initial state of the Enigma Machine"
      )

parserReflector :: Parser SerialNumber
parserReflector
  = option readIntegral
      (  long "reflector"
      <> short 'r'
      <> metavar "<SerialNumber>"
      <> help "reflectors serial number"
      )

data Rots = Rots
  { rot1 :: SerialNumber
  , rot2 :: SerialNumber
  , rot3 :: SerialNumber
  } deriving Show

parserSerial :: String -> Char -> Parser SerialNumber
parserSerial lon sho
  = option readIntegral
      (  long lon
      <> short sho
      <> metavar "<SerialNumber>"
      <> help (lon ++ " serial number")
      )

parserRot1 :: Parser SerialNumber
parserRot1 = parserSerial "first-rotor" '1'

parserRot2 :: Parser SerialNumber
parserRot2 = parserSerial "second-rotor" '2'

parserRot3 :: Parser SerialNumber
parserRot3 = parserSerial "third-rotor" '3'

parserRots :: Parser Rots
parserRots = Rots <$> parserRot1 <*> parserRot2 <*> parserRot3

type EnigmaSpecFile = FilePath

configHelp :: String
configHelp = "ini file"

parserEnigmaSpecFile :: Parser EnigmaSpecFile
parserEnigmaSpecFile
   = strOption
       (  long "config-file"
       <> short 'c'
       <> metavar "<ConfigFile>"
       <> help configHelp
       )

data IOSpec = IOSpec
  { inputSpec  :: InputSpec
  , outputSpec :: OutputSpec
  } deriving Show

parserIOSpec :: Parser IOSpec
parserIOSpec = IOSpec <$> parserInputSpec <*> parserOutputSpec

type InputSpec = Maybe FilePath

stdInput :: InputSpec
stdInput = Nothing

inputHelp :: String
inputHelp = "input file, default is <stdin>"

parserInputStd :: Parser InputSpec
parserInputStd = pure stdInput

parserInputFile :: Parser InputSpec
parserInputFile
   =  Just
  <$> strOption
        (  long "input-file"
        <> short 'i'
        <> metavar "<InputFile>"
        <> help inputHelp
        )

parserInputSpec :: Parser InputSpec
parserInputSpec = parserInputFile <|> parserInputStd

type OutputSpec = Maybe FilePath

stdOutput :: OutputSpec
stdOutput = Nothing

outputHelp :: String
outputHelp = "output file, default is <stdout>"

parserOutputStd :: Parser OutputSpec
parserOutputStd = pure stdOutput

parserOutputFile :: Parser OutputSpec
parserOutputFile
   = Just
  <$> strOption
        (  long "output-file"
        <> short 'o'
        <> metavar "<OutputFile>"
        <> help outputHelp
        )

parserOutputSpec :: Parser OutputSpec
parserOutputSpec = parserOutputFile <|> parserOutputStd
