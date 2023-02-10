{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Options.Interpret
  ( runArgs
  , exitFail
  ) where

import Text.RawString.QQ
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative (Alternative(..))
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)

import System.Exit (ExitCode(..), exitWith)
import System.IO (IOMode(..), openFile, stdout, stdin)

import Enigma.Constants
import Enigma.Aliases
import Enigma.Reflector (Reflector, nthFactoryReflector)
import Enigma.Magazine (Magazine, initMagazine)
import Enigma.Enigma (Enigma, initEnigma)
import Options.Parse (Option(..), parseOptions)

bounds :: Text
bounds = mconcat
  [ "1. Alphabet in use:\n", T.pack $ show alphabet, "\n\n"
  , "2. Alphabet size (n):\n", T.pack $ show rotorSize, "\n\n"
  , "3. Max rotor serial number (n!):\n", T.pack $ show maxRotorNumber , "\n\n"
  , "4. Max reflector serial number (n!/(n/2)!*2^(n/2)):\n", T.pack $ show maxReflectorNumber , "\n\n"
  , "5. Amount of rotors (m):\n", T.pack $ show rotorsCount, "\n\n"
  , "6. Max state number of enigma (n^m):\n", T.pack $ show maxMagazineState, "\n"
  ]

usage :: Text
usage = [r|enigma <RequiredArgs> [Options]
  Enigma is a command line program that (inefficiently) encrypts
  your data either from a file or from <stdin>, and writes its output
  to a specified file or to <stdout>. It does not abide by the
  constraint of 26 symbols from the original hardware implementation,
  and it's capable to map all ASCII characters from printable range
  except for \delete character which was replaced with newline character.
  Options:
    -h,--help
            print this help message

    -b,--bounds
            print max bounds of the enigma machine parameters,
            and other additional information

    -i<InpFile>, --input=<InpFile>
            input file, default is <stdin>

    -o<OutFile>, --output=<OutFile>
            output file, default is <stdout>

  Required arguments:
    -r<n1>,<n2>,<n3>, --rotors=<n1>,<n2>,<n3>
            rotors serial numbers

    -e<num>, --reflector=<num>
            reflector serial number

    -s<num>, --state=<num>
            state of enigma machine
|]

iHelp :: Option -> Maybe (IO ())
iHelp Help = Just $ T.putStrLn usage >>exitWith ExitSuccess
iHelp _ = Nothing

iBounds :: Option -> Maybe (IO ())
iBounds Bounds = Just $ T.putStrLn bounds >> exitWith ExitSuccess
iBounds _ = Nothing

iRoSerial :: IORef (Maybe Magazine) -> Option -> Maybe (IO ())
iRoSerial ref (RoSerial (s1,s2,s3)) = Just $
  writeIORef ref $ Just $ initMagazine s1 s2 s3
iRoSerial _ _ = Nothing

iReSerial :: IORef (Maybe Reflector) -> Option -> Maybe (IO ())
iReSerial ref (ReSerial n) = Just $
  writeIORef ref $ Just $ nthFactoryReflector n
iReSerial _ _ = Nothing

iEState :: IORef (Maybe StateNumber) -> Option -> Maybe (IO ())
iEState ref (EState s) = Just $ do writeIORef ref $ Just s
iEState _ _ = Nothing

iInput :: IORef (Maybe IFilePath) -> Option -> Maybe (IO ())
iInput ref (Input f) = Just $ writeIORef ref $ Just f
iInput _ _ = Nothing

iOutput :: IORef (Maybe OFilePath) -> Option -> Maybe (IO ())
iOutput ref (Output f) = Just $ writeIORef ref $ Just f
iOutput _ _ = Nothing

exitFail :: IO ()
exitFail = do
  T.putStrLn usage
  exitWith $ ExitFailure 1

interpretOption
  :: IORef (Maybe Magazine)
  -> IORef (Maybe Reflector)
  -> IORef (Maybe StateNumber)
  -> IORef (Maybe IFilePath)
  -> IORef (Maybe OFilePath)
  -> Option
  -> IO ()
interpretOption rm rr rs ri ro o = do
  let mact = iHelp o
          <|> iBounds o
          <|> iRoSerial rm o
          <|> iReSerial rr o
          <|> iEState rs o
          <|> iInput ri o
          <|> iOutput ro o
  case mact of
    Just act -> act
    Nothing  -> exitFail

liftA5
  :: Applicative f
  => (a1 -> a2 -> a3 -> a4 -> a5 -> b)
  -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f b
liftA5 f a1 a2 a3 a4 a5 = f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5

interpretOptions :: [Option] -> IO (Maybe (Enigma, IHandle, OHandle))
interpretOptions os = do
  let nw = newIORef Nothing
  (rm,rr,rs,ri,ro) <- liftA5 (,,,,) nw nw nw nw nw
  traverse_ (interpretOption rm rr rs ri ro) os
  let rd = readIORef
  (mm,mr,ms,mi,mo) <- liftA5 (,,,,) (rd rm) (rd rr) (rd rs) (rd ri) (rd ro)
  case (mm,mr,ms) of
    (Just ma, Just re, Just st) -> do
      let e = initEnigma re ma st
      i <- maybe (pure stdin)  (flip openFile ReadMode) mi
      o <- maybe (pure stdout) (flip openFile WriteMode) mo
      return $ Just (e,i,o)
    _ -> return Nothing

runArgs :: [String] -> IO (Maybe (Enigma, IHandle, OHandle))
runArgs args = do
  case parseOptions args of
    Nothing -> return Nothing
    Just os -> interpretOptions os
