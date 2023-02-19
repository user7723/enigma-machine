{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Enigma.Options.ConfigFile
  ( readConfig
  ) where

import Enigma.Aliases
import Enigma.Options.Parse
  ( EnigmaSpec(..)
  , EnigmaSpecOpt(..)
  , Rots(..)
  )

import Control.Monad (void)
import Data.Void
import System.Exit (exitWith, ExitCode(..))

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as E
import qualified Control.Monad.Combinators as C
import Control.Monad.Combinators ((<|>))

import Data.Text (Text)
import qualified Data.Text.IO as T

import Data.List (nub, sort)

data EnigmaParam = Ro Rots | Re SerialNumber | St StateNumber

instance Show EnigmaParam where
  show (Re re) = "Reflector:\n" ++ show re
  show (St st) = "State:\n" ++ show st
  show (Ro Rots{..}) = concat
    [ "Rotor1:\n" , show rot1 , "\n"
    , "Rotor2:\n" , show rot2 , "\n"
    , "Rotor3:\n" , show rot3
    ]

instance Eq EnigmaParam where
  Ro _ == Ro _ = True
  Re _ == Re _ = True
  St _ == St _ = True
  _    == _    = False

instance Ord EnigmaParam where
  compare (Ro _) (Ro _) = EQ
  compare (Re _) (Re _) = EQ
  compare (St _) (St _) = EQ

  compare (Ro _) _      = LT
  compare (Re _) (Ro _) = GT
  compare (Re _) (St _) = LT
  compare (St _) _      = GT

readConfig :: FilePath -> IO EnigmaSpec
readConfig cf = do
  i  <- T.readFile cf
  case M.parse parseConfig cf i of
    Right es -> pure $ EnigmaSpecO es
    Left e   -> putStrLn (E.errorBundlePretty e)
             >> exitWith (ExitFailure 1)

type Parser = M.Parsec Void Text

guardParams :: [EnigmaParam] -> Parser EnigmaSpecOpt
guardParams ((Ro ro):(Re re):(St st):_) = return $ EnigmaSpecOpt ro re st
guardParams ps
  = fail msg
  where
    l = length ps
    msg = concat
      [ "you've provided not enough parameters there should be three, but "
      , show l , " was given. "
      , "Only those were read from the file: \n"
      , unlines $ map show ps
      ]

parseConfig :: Parser EnigmaSpecOpt
parseConfig = do
  void $ space
  C.many parseEnigmaParam >>= guardParams . sort . nub

parseEnigmaParam :: Parser EnigmaParam
parseEnigmaParam
   =  lexeme
   $  fmap Ro parseRotorNumbers
  <|> fmap Re parseReflectorNumber
  <|> fmap St parseStateNumber

parseRotorNumbers :: Parser Rots
parseRotorNumbers = lexeme $ do
  void $ parseTag "rotors" "ro"
  C.many parseNumber >>= \case
    (r1:r2:r3:_) -> return $ Rots
      { rot1 = r1
      , rot2 = r2
      , rot3 = r3
      }
    _            -> fail "expected three serial numbers for Enigma rotors"

parseReflectorNumber :: Parser SerialNumber
parseReflectorNumber = lexeme $ parseTag "reflector" "re" >> parseNumber

parseStateNumber :: Parser StateNumber
parseStateNumber = lexeme $ parseTag "state" "st" >> parseNumber

parseNumber :: (Read a, Integral a) => Parser a
parseNumber = lexeme $ do
  n <- C.some M.digitChar
  return $ read n

type Tag = Text
type Alias = Text

parseTag :: Tag -> Alias -> Parser Text
parseTag t a = lexeme (M.string' t <|> M.string' a)

space :: Parser ()
space = L.space
  M.space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*"  "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
