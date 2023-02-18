{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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

readConfig :: FilePath -> IO EnigmaSpec
readConfig cf = do
  i  <- T.readFile cf
  case M.parse parseConfig cf i of
    Right es -> pure $ EnigmaSpecO es
    Left e   -> putStrLn (E.errorBundlePretty e)
             >> exitWith (ExitFailure 1)

type Parser = M.Parsec Void Text

parseConfig :: Parser EnigmaSpecOpt
parseConfig = do
  void $ space
  ro <- parseRotorNumbers
  re <- parseReflectorNumber
  st <- parseStateNumber
  pure $ EnigmaSpecOpt ro re st

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
