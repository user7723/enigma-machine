module Options.Parse
  ( Option(..)
  , parseOptions
  ) where

import qualified Data.Text as T
import qualified Data.Text.Read as T

import Data.List (stripPrefix)
import Control.Applicative (Alternative(..))
import Control.Monad (when)


import Enigma.Aliases

data Option
  = Help
  | Bounds
  | RoSerial (SerialNumber, SerialNumber, SerialNumber)
  | ReSerial SerialNumber
  | EState StateNumber
  | Input FilePath
  | Output FilePath
  deriving Show

parseHelp :: String -> Maybe Option
parseHelp s
  = const Help
 <$> (stripPrefix "-h" s
 <|>  stripPrefix "--help" s)

parseBounds :: String -> Maybe Option
parseBounds s
  = const Bounds
 <$> (stripPrefix "-b" s
 <|>  stripPrefix "--bounds" s)

parseInputFile :: String -> Maybe Option
parseInputFile s
  = Input
 <$> (stripPrefix "-i" s
 <|>  stripPrefix "--input=" s)

parseOutputFile :: String -> Maybe Option
parseOutputFile s
  =  Output
 <$> (stripPrefix "-o" s
 <|>  stripPrefix "--output=" s)

parseRoSerial :: String -> Maybe Option
parseRoSerial o = do
  s <- stripPrefix "-r" o <|> stripPrefix "--rotors=" o
  let ts = T.split (== ',') $ T.pack s
  -- lets just fix it to a magic number for now
  when (length ts /= 3) empty
  either
    (const Nothing)
    (Just . RoSerial . mkTriple . map fst)
    $ traverse T.decimal ts
  where
    mkTriple (a:b:c:_) = (a,b,c)
    mkTriple _ = error "you've probalby forgot about list size invariant"

parseReSerial :: String -> Maybe Option
parseReSerial o = do
  s <- stripPrefix "-e" o <|> stripPrefix "--reflector=" o
  either
    (const Nothing)
    (Just . ReSerial . fst)
    $ T.decimal $ T.pack s

parseEState :: String -> Maybe Option
parseEState o = do
  s <- stripPrefix "-s" o <|> stripPrefix "--state=" o
  either
    (const Nothing)
    (Just . EState . fst)
    $ T.decimal $ T.pack s

parseOption :: String -> Maybe Option
parseOption s =  parseHelp s
             <|> parseBounds s
             <|> parseRoSerial s
             <|> parseReSerial s
             <|> parseEState s
             <|> parseInputFile s
             <|> parseOutputFile s

parseOptions :: [String] -> Maybe [Option]
parseOptions = traverse parseOption
