module Options.Interact
  ( interactiveReadSpec
  ) where

import Data.Either (either)
import Text.Read (readEither)
import System.IO (hSetBuffering, BufferMode(..), stdout)

import Enigma.Aliases
  ( SerialNumber
  , StateNumber
  , IHandle
  , OHandle
  )
import Enigma.Magazine (Magazine, initMagazine)
import Enigma.Enigma (Enigma, initEnigma)
import Enigma.Reflector (Reflector, nthFactoryReflector)
import Enigma.Rotor (Rotor, nthFactoryRotor)
import Enigma.Constants
  ( maxRotorNumber
  , maxReflectorNumber
  , maxMagazineState
  )

interactiveReadSpec :: IO Enigma
interactiveReadSpec = do
  hSetBuffering stdout NoBuffering
  putStrLn "Note: for every parameter there will be shown maximum bound value, if you'll exceed it, then the your input will be taken by modulo of that maximum value.\n"
  (r1,r2,r3) <- askRotorNumbers
  reflectorN <- askReflectorNumber
  stateN     <- askStateNumber
  let mag = initMagazine r1 r2 r3
      ref = nthFactoryReflector reflectorN
      enigma = initEnigma ref mag stateN
  return enigma

data RotNum = I | II | III
instance Show RotNum where
  show (I) = "rot1"
  show (II) = "rot2"
  show (III) = "rot3"

askRotorNumbers :: IO (SerialNumber, SerialNumber, SerialNumber)
askRotorNumbers = do
  putStrLn "You need to enter three serial numbers for Enigma machine's rotors. Maximum value that you can provide looks exactly like this:"
  putStrLn $ show maxRotorNumber
  r1 <- getRotorSerial I
  r2 <- getRotorSerial II
  r3 <- getRotorSerial III
  return (r1, r2, r3)

askReflectorNumber :: IO SerialNumber
askReflectorNumber = do
  putStrLn "Now insert a serial number for the Reflector that is going to be used. It's maximum value is:"
  putStrLn $ show maxReflectorNumber
  getReflectorSerial >>= return 

askStateNumber :: IO StateNumber
askStateNumber = do
  putStrLn "The last thing you'll need to do before beginning the encryption process is to set up the initial state of the rotors that you've chosen. The max state value is:"
  putStrLn $ show maxMagazineState
  getStateNumber >>= return

type Prompt = String

getNumber :: (Read a, Integral a) => Prompt -> IO a
getNumber p = do
  putStr p
  ln <- getLine 
  either
    (const $ getNumber p)
    return
    (readEither ln)

getRotorSerial :: RotNum -> IO SerialNumber
getRotorSerial n = getNumber (show n ++ " > ")

getReflectorSerial :: IO SerialNumber
getReflectorSerial = getNumber "ref > "

getStateNumber :: IO StateNumber
getStateNumber = getNumber "state > "
