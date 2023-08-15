{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Model ( Symbol(..)
             , Envelope(..)
             , createEnvelope
             , Expression(..)
             , evaluate ) where

import            Data.Aeson   hiding (json)
import            Data.UUID
import            GHC.Generics
import            Data.UUID.V4 (nextRandom)

-- Symbol
data Symbol
  = PLUS | MINUS | MULT | DIV
  deriving (Generic, Show, Enum, Eq)

-- JSON
instance ToJSON Symbol
instance FromJSON Symbol

-- Expression
data Expression = Val Integer | Ex Expression Expression Symbol
  deriving (Generic, Show, Eq)

-- JSON
instance ToJSON Expression
instance FromJSON Expression

----------------------------------
--          Evaluator           --
----------------------------------

evaluate :: Expression -> Integer
evaluate (Val x)    = x
evaluate (Ex a b s) = case s of
  PLUS  -> evaluate a + evaluate b
  MINUS -> evaluate a - evaluate b
  MULT  -> evaluate a * evaluate b
  DIV   -> evaluate a `div` evaluate b

----------------------------------
-- Parametric Identifiable Data --
----------------------------------
data Envelope a where
  Envelope :: (ToJSON a, FromJSON a) => {eId :: UUID, eValue :: a} -> Envelope a

deriving instance (Eq a) => Eq (Envelope a)
deriving instance (Show a) => Show (Envelope a)
deriving instance (ToJSON a, FromJSON a, Read a) => Read (Envelope a)

createEnvelope :: (ToJSON a, FromJSON a) => a -> IO (Envelope a)
createEnvelope a = do
  generatedId <- nextRandom
  return Envelope { eId = generatedId, eValue = a }

instance ToJSON (Envelope a) where
  toJSON (Envelope envelopeId envelopeValue) = let
    idJson = toJSON envelopeId
    valueJson = toJSON envelopeValue
      in object ["id" .= idJson, "value" .= valueJson]

instance (ToJSON a, FromJSON a) => FromJSON (Envelope a) where
  parseJSON = withObject "Envelope" $ \v -> Envelope
    <$> v .: "id"
    <*> v .: "value"
