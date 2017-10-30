{-# LANGUAGE DeriveDataTypeable #-}

module Cacco.Expr
  ( Expr(..)

  ) where

import           Data.Monoid     ((<>))
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)

import           Cacco.Location

type Name = String

data Expr
  -- Atomic Values
  -- | Undefined literal
  = Undef Location
  -- | Boolean literal
  | Boolean Location Bool
  -- | Integer literal
  | Integer Location Integer
  -- | Decimal literal
  | Decimal Location Scientific
  -- | String literal
  | String Location Text
  -- | Variable or Constant symbol
  | Symbol Location Name

  -- Collections
  -- | List
  | List Location [Expr]
  -- | Vector
  | Vector Location [Expr]
  deriving (Eq, Ord, Typeable)

instance Show Expr where
  show expr = case expr of
    Undef     l    -> "Undef (" <> show l <> ")"
    Boolean   l x  -> "Boolean " <> show x <> " (" <> show l <> ")"
    Integer   l x  -> "Integer " <> show x <> " (" <> show l <> ")"
    Decimal   l x  -> "Decimal " <> show x <> " (" <> show l <> ")"
    String    l x  -> "String " <> show x <> " (" <> show l <> ")"
    Symbol    l x  -> "Symbol " <> x <> " (" <> show l <> ")"
    List      l xs -> "(" <> unwords (show <$> xs) <> ") (" <> show l <> ")"
    Vector    l xs -> "[" <> unwords (show <$> xs) <> "] (" <> show l <> ")"

instance HasLocation Expr where
  getLocation expr = case expr of
    Undef     l   -> l
    Boolean   l _ -> l
    Integer   l _ -> l
    Decimal   l _ -> l
    String    l _ -> l
    Symbol    l _ -> l
    List      l _ -> l
    Vector    l _ -> l
