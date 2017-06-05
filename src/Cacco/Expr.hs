{-# LANGUAGE DeriveDataTypeable #-}

module Cacco.Expr
  ( Expr(..)

  ) where
import           Data.Foldable   (foldl')
import           Data.List       (intercalate)
import           Data.Monoid     ((<>))
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           Prelude         hiding (False, True)

import           Cacco.Location

type Name = String

data Expr
  -- | Undefined literal
  = Undef   Location
  -- | Boolean literal
  | Boolean Location Bool
  -- | Integer literal
  | Integer Location Integer
  -- | Decimal literal
  | Decimal Location Scientific
  -- | String literal
  | String Location Text
  -- | Variable or Constant symbol
  | Atom Location Name
  -- | List
  | List Location [Expr]
  -- | Procedure
  | Proc Location [Name] Expr
  -- | Apply procedure
  | Call  Location Expr [Expr]
  deriving (Eq, Ord, Typeable)

instance Show Expr where
  show (Undef l) =
    "Undef(" <> show l <> ")"

  show (Boolean l x) =
    "Boolean " <> show x <> " (" <> show l <> ")"

  show (Integer l x) =
    "Integer " <> show x <> " (" <> show l <> ")"

  show (Decimal l x) =
    "Decimal " <> show x <> " (" <> show l <> ")"

  show (String  l x) =
    "String " <> show x <> " (" <> show l <> ")"

  show (Atom l x) =
    "Symbol " <> x <> " (" <> show l <> ")"

  show (List l xs) =
    "[" <> unwords (show <$> xs) <> "] (" <> show l <> ")"

  show (Proc l ns xp) =
    "(|" <> unwords ns <> "| " <> show xp <> ") (" <> show l <> ")"

  show (Call l fn xps) =
    "(" <> show fn <> " " <> unwords (show <$> xps) <>　"(" <> show l <> ")"
