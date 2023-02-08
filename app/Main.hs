module Main where

import Enigma.Enigma     (initEnigma)
import Enigma.Reflector  (nthFactoryReflector)
import Enigma.Magazine   (initMagazine)
import Enigma.Encryption (encrypt, decrypt)

enigmaSample =
  initEnigma
    (nthFactoryReflector 0)
    (initMagazine 12933993 912391293 2939232)
    0

main :: IO ()
main = pure ()
