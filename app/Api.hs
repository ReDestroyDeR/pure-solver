{-# LANGUAGE OverloadedStrings #-}
module Api (rest
          , Session(..)
          , AppState(..)) where

import           Model

import           Web.Spock
import           Data.IORef             (IORef, readIORef)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

data Session = EmptySession
newtype AppState = DummyAppState (IORef [Equation])

rest :: SpockM () Session AppState ()
rest =
  do
      get root $ do
         (DummyAppState ref) <- getState
         equations <- liftIO $ readIORef ref
         json equations
      get ("test" <//> var) $ \x -> text $ T.pack $ show [1..x :: Integer]