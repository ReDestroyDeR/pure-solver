{-# LANGUAGE OverloadedStrings #-}
module Api ( rest
           , Session(..)
           , AppState(..) ) where

import           Model

import           Web.Spock
import           Data.IORef             (IORef, modifyIORef', readIORef)
import           Control.Monad.IO.Class (liftIO)

data Session = EmptySession
newtype AppState = DummyAppState (IORef [Envelope Equation])

type Api = SpockM () Session AppState ()
type ApiAction a = SpockAction () Session AppState a

rest :: Api
rest =
  do
    get ("api" <//> "v1" <//> "equations") $ do
      (DummyAppState ref) <- getState
      equations <- liftIO $ readIORef ref
      json equations
    post ("api" <//> "v1" <//> "equations") $ do
      equation <- jsonBody' :: ApiAction Equation
      (DummyAppState ref) <- getState
      envelope <- liftIO $ createEnvelope equation
      liftIO $ modifyIORef' ref (envelope :)
      redirect "/api/v1/equations"
    put ("api" <//> "v1" <//> "equations" <//> var) $ \equationId -> do
      (DummyAppState ref) <- getState
    delete ("api" <//> "v1" <//> "equations" <//> var) $ \equationId -> do
      (DummyAppState ref) <- getState

