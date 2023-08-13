{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model (Symbol(..)
            , Expression(..)
            , Equation(..)) where

import            Data.Aeson   hiding (json)
import            Data.Monoid  ((<>))
import            Data.Text    (Text, pack)
import            GHC.Generics

-- Symbol
data Symbol 
  = PLUS | MINUS | MULT | DIV
  deriving (Generic, Show, Enum, Eq)

-- JSON
instance ToJSON Symbol
instance FromJSON Symbol

-- Expression
data Expression
  = Expression { x :: Integer
               , y :: Integer
               , operator :: Symbol }
  deriving (Generic, Show, Eq)

-- JSON
instance ToJSON Expression
instance FromJSON Expression

-- Equation
newtype Equation
  = Equation [Expression]
  deriving (Generic, Show, Eq)
  
-- JSON
instance ToJSON Equation
instance FromJSON Equation
