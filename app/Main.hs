module Main where

import Enigma.Enigma     (initEnigma)
import Enigma.Reflector  (nthFactoryReflector)
import Enigma.Magazine   (initMagazine)
import Enigma.Encryption (encrypt, decrypt)

enigma =
  initEnigma
    (nthFactoryReflector 0)
    (initMagazine 12933993 912391293 2939232)
    83838

main :: IO ()
main = do
  let msg = "Hello, world!"
      emsg = encrypt enigma msg
  print msg
  print emsg
  let msg' = decrypt enigma emsg
  print msg'
