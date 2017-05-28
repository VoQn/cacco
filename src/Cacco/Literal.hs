{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}

module Cacco.Literal
  ( Literal(..)
  ) where

import           Data.Monoid     ((<>))
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)

import           Cacco.Location  (HasLocation (..), Location)

-- | Literal Data for cacco
data Literal
  = IntegerLiteral Location Integer
  | DecimalLiteral Location Scientific
  | StringLiteral  Location Text
  deriving (Eq, Ord, Typeable)

instance Show Literal where
  show = \case
    IntegerLiteral l x -> "Integer " <> show x <> " (" <> show l <> ")"
    DecimalLiteral l x -> "Decimal " <> show x <> " (" <> show l <> ")"
    StringLiteral  l x -> "String "  <> show x <> " (" <> show l <> ")"

-- | Get the location of the literal.
instance HasLocation Literal where
  getLocation = \case
    IntegerLiteral l _ -> l
    DecimalLiteral l _ -> l
    StringLiteral  l _ -> l
