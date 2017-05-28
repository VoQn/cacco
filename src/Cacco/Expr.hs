{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
module Cacco.Expr
  ( Expr(..)

  ) where
import           Data.Foldable   (foldl')
import           Data.Monoid     ((<>))
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           Prelude         hiding (False, True)

import           Cacco.Location

type Name = String

data Expr
  = Undef   Location
  | True    Location
  | False   Location
  | Integer Location Integer
  | Decimal Location Scientific
  | String  Location Text
  | Atom    Location Name
  | List    Location [Expr]
  deriving (Eq, Ord, Typeable)

instance Show Expr where
  show = \case
    Undef l -> "Undefined(" <> show l <> ")"
    True  l -> "True(" <> show l <> ")"
    False l -> "False(" <> show l <> ")"
    Integer l x -> "Integer " <> show x <> "(" <> show l <> ")"
    Decimal l x -> "Decimal " <> show x <> "(" <> show l <> ")"
    String  l x -> "String " <> show x <> "(" <> show l <> ")"
    Atom    l x -> "Symbol " <> x <> "(" <> show l <> ")"
    List    l xs -> "List[" <> foldl' (\acc x -> acc <> " " <> show x) "" xs <> "](" <> show l <> ")"
