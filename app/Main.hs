module Main (main) where

import Api
import Model
import Web.Spock
import Web.Spock.Config

import Data.IORef

main :: IO ()
main =
  do ref <- newIORef [Equation [Expression 3 4 PLUS, Expression 7 8 MINUS]]
     spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
     runSpock 8080 (spock spockCfg rest)



