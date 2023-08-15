module Main (main) where

import Api
import Model
import Web.Spock
import Web.Spock.Config

import Data.IORef

main :: IO ()
main =
  do exampleEquation <- createEnvelope $
              Ex
                (Ex (Val 3) (Val 4) PLUS)
                (Ex (Val 8) (Val 3) MINUS)
                MULT
     ref <- newIORef [exampleEquation]
     spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
     runSpock 8080 (spock spockCfg rest)



