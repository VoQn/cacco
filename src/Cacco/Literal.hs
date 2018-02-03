module Cacco.Literal
  ( Literal(..)
  ) where

import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)

data Literal
  = Undefined
  | Boolean Bool
  | Integer Integer
  | Decimal Scientific
  | Text    Text
  deriving (Eq, Ord, Show, Typeable)