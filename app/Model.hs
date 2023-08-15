{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Model ( Symbol(..)
             , Envelope(..)
             , createEnvelope
             , Expression(..)
             , evaluate ) where

import            Data.Aeson   hiding (json)
import            Data.UUID
import            GHC.Generics
import            Data.UUID.V4 (nextRandom)
import            Data.Scientific
import            Data.Maybe (fromMaybe)
import Data.Aeson.Types (Parser)

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

fromScientific :: Scientific -> Parser Int
fromScientific s = case toBoundedInteger s of
  Just x -> pure x
  Nothing -> fail "Переданное число вышло за максимально допустимые границы"

-- JSON
instance ToJSON Expression where
  toJSON (Val x) = toJSON x
  toJSON (Ex a b s) = object ["x" .= toJSON a, "y" .= toJSON b, "operator" .= toJSON s]

instance FromJSON Expression where
    parseJSON (Number n) | isInteger n = fmap (Val . toInteger) (fromScientific n)
                         | otherwise   = fail "Не целое число"
    parseJSON (Object o) = Ex
      <$> o .: "x"
      <*> o .: "y"
      <*> o .: "operator"
    parseJSON x = fail $ "Не могу обработать : " ++ show x

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
