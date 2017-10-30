module Cacco.Ast
  ( Ast(..)
  , fromExpr
  ) where

import           Data.Functor    ((<$>))
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)

import           Cacco.Expr      (Expr)
import qualified Cacco.Expr      as Expr

data Ast
  -- | Undefined literal
  = Undefined
  | Boolean Bool
  | Integer Integer
  | Decimal Scientific
  | String  Text
  | Symbol  String
  | List    [Ast]
  | Vector  [Ast]
  deriving (Eq, Ord, Show, Typeable)

fromExpr :: Expr -> Ast
fromExpr expr = case expr of
  Expr.Undef   _    -> Undefined
  Expr.Boolean _ x  -> Boolean x
  Expr.Integer _ x  -> Integer x
  Expr.Decimal _ x  -> Decimal x
  Expr.String  _ x  -> String x
  Expr.Atom    _ x  -> Symbol x
  Expr.List    _ xs -> List $ fromExpr <$> xs
  Expr.Vector  _ xs -> Vector $ fromExpr <$> xs
