{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}

module Cacco.Literal
  ( Literal(..)
  , getLocation
  ) where

import           Data.Monoid     ((<>))
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)

import           Cacco.Location  (Location)

-- | Literal Data for cacco
data Literal
  = IntegerLiteral Integer    Location
  | DecimalLiteral Scientific Location
  | StringLiteral  Text       Location
  deriving (Eq, Ord, Typeable)

instance Show Literal where
  show = \case
    IntegerLiteral x l -> "Integer " <> show x <> " (" <> show l <> ")"
    DecimalLiteral x l -> "Decimal " <> show x <> " (" <> show l <> ")"
    StringLiteral  x l -> "String "  <> show x <> " (" <> show l <> ")"

-- | Get the location of the literal.
getLocation :: Literal -> Location
getLocation = \case
  IntegerLiteral _ l -> l
  DecimalLiteral _ l -> l
  StringLiteral  _ l -> l
