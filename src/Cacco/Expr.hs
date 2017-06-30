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
  show expr = case expr of
    Undef    l      -> "Undef (" <> show l <> ")"
    Boolean  l x    -> "Boolean " <> show x <> " (" <> show l <> ")"
    Integer  l x    -> "Integer " <> show x <> " (" <> show l <> ")"
    Decimal  l x    -> "Decimal " <> show x <> " (" <> show l <> ")"
    String   l x    -> "String " <> show x <> " (" <> show l <> ")"
    Atom     l x    -> "Symbol " <> x <> " (" <> show l <> ")"
    List     l xs   -> "[" <> unwords (show <$> xs) <> "] (" <> show l <> ")"
    Declare  l n x  -> "(dec " <> n <> " " <> show x <> ") (" <> show l <> ")"
    Let      l n x  -> "(let " <> n <> " " <> show x <> ") (" <> show l <> ")"
    Var      l n x  -> "(var " <> n <> " " <> show x <> ") (" <> show l <> ")"
    Reassign l n x  -> "(set! " <> n <> " " <> show x <> ") (" <> show l <> ")"
    Proc     l ns x -> "(|" <> unwords ns <> "| " <> show x <> ") (" <> show l <> ")"
    Call     l f xs -> "(" <> show f <> " " <> unwords (show <$> xs) <>　"(" <> show l <> ")"

instance HasLocation Expr where
  getLocation expr = case expr of
    Undef    l     -> l
    Boolean  l _   -> l
    Integer  l _   -> l
    Decimal  l _   -> l
    String   l _   -> l
    Atom     l _   -> l
    List     l _   -> l
    Declare  l _ _ -> l
    Let      l _ _ -> l
    Var      l _ _ -> l
    Reassign l _ _ -> l
    Proc     l _ _ -> l
    Call     l _ _ -> l



