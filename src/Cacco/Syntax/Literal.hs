{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cacco.Syntax.Literal
  ( Literal(..)

  ) where

import           Control.DeepSeq           (NFData)
import           Data.Data                 (Data)
import           Data.Scientific           (Scientific)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (..), dquotes, (<>))
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Numeric.Natural           (Natural)

data Literal
  = Undef
  | Unit
  | Bool Bool

  -- Signed integers
  | Int8    Integer
  | Int16   Integer
  | Int32   Integer
  | Int64   Integer
  | Integer Integer

  -- Unsigned integers
  | Uint8   Natural
  | Uint16  Natural
  | Uint32  Natural
  | Uint64  Natural
  | Numeric Natural

  -- Floating point numbers
  | Float16 Scientific
  | Float32 Scientific
  | Float64 Scientific
  | Flonum  Scientific

  -- Text
  | Text Text
  deriving (Eq, Show, Data, Typeable, Generic)

instance NFData Literal

instance Pretty Literal where
  pretty lit = case lit of
    Undef      -> "undefined"
    Unit       -> "()"
    Bool True  -> "true"
    Bool False -> "false"
    Int8 x     -> pretty x <> "_i8"
    Int16 x    -> pretty x <> "_i16"
    Int32 x    -> pretty x <> "_i32"
    Int64 x    -> pretty x <> "_i64"
    Uint8 x    -> pretty x <> "_u8"
    Uint16 x   -> pretty x <> "_u16"
    Uint32 x   -> pretty x <> "_u32"
    Uint64 x   -> pretty x <> "_u64"
    Integer x  -> (if x < 0 then "" else "+") <> pretty x
    Numeric x  -> pretty x
    Float16 x  -> pretty (show x) <> "_f16"
    Float32 x  -> pretty (show x) <> "_f32"
    Float64 x  -> pretty (show x) <> "_f64"
    Flonum  x  -> pretty (show x)
    Text    x  -> dquotes $ pretty x
