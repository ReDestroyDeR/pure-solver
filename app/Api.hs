{-# LANGUAGE OverloadedStrings #-}
module Api ( rest
           , Session(..)
           , AppState(..) ) where

import           Model

import           Web.Spock
import           Data.IORef             (IORef, atomicModifyIORef', readIORef)
import           Control.Monad.IO.Class (liftIO)
import           Data.UUID
import           Data.Aeson.Types (ToJSON, FromJSON)

data Session = EmptySession
newtype AppState = DummyAppState (IORef [Envelope Expression])

type Api = SpockM () Session AppState ()
type ApiAction a = SpockAction () Session AppState a

swapEnvelopeById :: (ToJSON a, FromJSON a) => UUID -> [Envelope a] -> a -> IO [Envelope a]
swapEnvelopeById i [] _ = fail $ "Не найдена запись с идентификатором " ++ show i
swapEnvelopeById i (x:xs) new
    | envelopeId x == i = pure $ Envelope i new:xs -- Заменить текущий элемент
    | otherwise         = fmap (x:) (swapEnvelopeById i xs new) -- Добавить следующие значения

rest :: Api
rest =
  do
    get ("api" <//> "v1" <//> "equations") $ do
      (DummyAppState ref) <- getState
      equations <- liftIO $ readIORef ref
      json equations
      
    post ("api" <//> "v1" <//> "equations") $ do
      equation <- jsonBody' :: ApiAction Expression
      (DummyAppState ref) <- getState
      envelope <- liftIO $ createEnvelope equation
      _ <- liftIO $ atomicModifyIORef' ref (\l -> (envelope:l, envelope:l))
      redirect "/api/v1/equations"
      
    put ("api" <//> "v1" <//> "equations" <//> var) $ \equationId -> do
      (DummyAppState ref) <- getState
      equation <- jsonBody' :: ApiAction Expression
      updated <- liftIO $ atomicModifyIORef' ref (\envelopes -> ( swapEnvelopeById equationId envelopes equation -- Не компилируется тут. 
                                                                , swapEnvelopeById equationId envelopes equation ))
      json updated
      
    delete ("api" <//> "v1" <//> "equations" <//> var) $ \equationId -> do
      (DummyAppState ref) <- getState
      text equationId
