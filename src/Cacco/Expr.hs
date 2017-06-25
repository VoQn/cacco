{-# LANGUAGE DeriveDataTypeable #-}

module Cacco.Expr
  ( Expr(..)

  ) where

import           Data.Monoid     ((<>))
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           Prelude         hiding (False, True)

import           Cacco.Location

type Name = String

data Expr
  -- Atomic Values
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

  -- Collections
  -- | List
  | List Location [Expr]

  -- Forms
  -- | Declare symbol's typing
  | Declare Location Name Expr
  -- | Let binding
  | Let Location Name Expr
  -- | Variable declaration
  | Var Location Name Expr
  -- | Reassign variables
  | Reassign Location Name Expr
  -- | Procedure
  | Proc Location [Name] Expr
  -- | Apply procedure
  | Call Location Expr [Expr]
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

  show (Declare l n x) =
    "(dec " <> n <> " " <> show x <> ") (" <> show l <> ")"

  show (Let l n x) =
    "(let " <> n <> " " <> show x <> ") (" <> show l <> ")"

  show (Var l n x) =
    "(var " <> n <> " " <> show x <> ") (" <> show l <> ")"

  show (Reassign l n x) =
    "(set! " <> n <> " " <> show x <> ") (" <> show l <> ")"

  show (Proc l ns xp) =
    "(|" <> unwords ns <> "| " <> show xp <> ") (" <> show l <> ")"

  show (Call l f xs) =
    "(" <> show f <> " " <> unwords (show <$> xs) <>　"(" <> show l <> ")"
