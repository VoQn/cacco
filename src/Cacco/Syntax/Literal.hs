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
    | Natural Natural

    -- Floating point numbers
    | Float16 Scientific
    | Float32 Scientific
    | Float64 Scientific
    | Flonum  Scientific

    -- Text
    | Text Text
    deriving (Eq, Show, Data, Typeable, Generic)

instance NFData Literal

instance Pretty Literal
  where
    pretty Undef       = "undefined"
    pretty Unit        = "()"
    pretty (Bool True) = "true"
    pretty (Bool ____) = "false"
    pretty (Int8    x) = pretty x <> "_i8"
    pretty (Int16   x) = pretty x <> "_i16"
    pretty (Int32   x) = pretty x <> "_i32"
    pretty (Int64   x) = pretty x <> "_i64"
    pretty (Uint8   x) = pretty x <> "_u8"
    pretty (Uint16  x) = pretty x <> "_u16"
    pretty (Uint32  x) = pretty x <> "_u32"
    pretty (Uint64  x) = pretty x <> "_u64"
    pretty (Integer x) = (if x < 0 then "" else "+") <> pretty x
    pretty (Natural x) = pretty x
    pretty (Float16 x) = pretty (show x) <> "_f16"
    pretty (Float32 x) = pretty (show x) <> "_f32"
    pretty (Float64 x) = pretty (show x) <> "_f64"
    pretty (Flonum  x) = pretty (show x)
    pretty (Text    x) = dquotes $ pretty x
