{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cacco.Syntax.Term where

import           Control.DeepSeq           (NFData)
import           Data.Char                 (isLetter)
import           Data.Data                 (Data)
import           Data.Int                  (Int16, Int32, Int64, Int8)
import           Data.Scientific           (Scientific, fromFloatDigits)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (..), dquotes, (<>))
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word16, Word32, Word64, Word8)
import           GHC.Generics              (Generic)
import           Numeric.Half              (Half, toHalf)
import           Numeric.Natural           (Natural)
import           Test.QuickCheck           (Arbitrary (..), CoArbitrary (..),
                                            Gen, arbitraryPrintableChar, oneof)
import           Test.QuickCheck.Gen       (listOf, suchThat)
import           Test.QuickCheck.Instances ()

data Term
    -- Specific literal
    = Hole      -- ^ Hole @_@
    | Dots      -- ^ Three Dots @...@
    | Nil       -- ^ Meaning "nothing" @nil@
    | Unit      -- ^ Unit Type @()@
    | Undefined -- ^ @undefined@
    -- Variable
    | Symbol  String -- ^ A identified symbol
    -- Boolean
    | Bool    Bool -- ^ Boolean Value @true@ or @false@
    -- Signed integers
    | Int8    Integer -- ^ 8bit signed integer
    | Int16   Integer -- ^ 16bit signed integer
    | Int32   Integer -- ^ 32bit signed integer
    | Int64   Integer -- ^ 64bit signed integer
    | Integer Integer -- ^ Arbitrary-precision signed integer
    -- Unsigned integers
    | Uint8   Natural -- ^ 8bit unsigned integer
    | Uint16  Natural -- ^ 16bit unsigned integer
    | Uint32  Natural -- ^ 32bit unsigned integer
    | Uint64  Natural -- ^ 64bit unsigned integer
    | Numeric Natural -- ^ Arbitrary-precision unsigned integer
    -- Floating point numbers
    | Float16 Scientific -- ^ 16bit floating point number (IEEE 254-2008 binary16)
    | Float32 Scientific -- ^ 32bit floating point number (IEEE 254-2008 binary32)
    | Float64 Scientific -- ^ 64bit floating point number (IEEE 254-2008 binary64)
    | Flonum  Scientific -- ^ Arbitrary-precision real number
    -- Text
    | Text    Text -- ^ Text literal
    deriving (Eq, Show, Data, Typeable, Generic)

instance NFData Term

instance Pretty Term where
    pretty term = case term of
        Hole       -> "_"
        Dots       -> "..."
        Undefined  -> "undefined"
        Nil        -> "nil"
        Unit       -> "()"
        Bool True  -> "true"
        Bool False -> "false"
        Symbol  n  -> pretty n
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

instance Arbitrary Half where
    arbitrary = toHalf <$> (arbitrary :: Gen Float)

instance Arbitrary Term where
    arbitrary = oneof
        [ pure Hole
        , pure Dots
        , pure Unit
        , pure Undefined
        , Symbol <$> symbolName
        , Bool <$> arbitrary
        , Int8  . fromIntegral <$> (arbitrary :: Gen Int8)
        , Int16 . fromIntegral <$> (arbitrary :: Gen Int16)
        , Int32 . fromIntegral <$> (arbitrary :: Gen Int32)
        , Int64 . fromIntegral <$> (arbitrary :: Gen Int64)
        , Integer <$> arbitrary
        , Uint8  . fromIntegral <$> (arbitrary :: Gen Word8)
        , Uint16 . fromIntegral <$> (arbitrary :: Gen Word16)
        , Uint32 . fromIntegral <$> (arbitrary :: Gen Word32)
        , Uint64 . fromIntegral <$> (arbitrary :: Gen Word64)
        , Numeric <$> arbitrary
        , Float16 . fromFloatDigits <$> (arbitrary :: Gen Half)
        , Float32 . fromFloatDigits <$> (arbitrary :: Gen Float)
        , Float64 . fromFloatDigits <$> (arbitrary :: Gen Double)
        , Flonum <$> arbitrary
        , Text <$> arbitrary
        ]
      where
        symbolName :: Gen String
        symbolName = (:) <$> identStart <*> identTrail
        {-# INLINE symbolName #-}

        identStart :: Gen Char
        identStart = arbitraryPrintableChar `suchThat` isLetter
        {-# INLINE identStart #-}

        identTrail :: Gen String
        identTrail = listOf arbitraryPrintableChar
        {-# INLINE identTrail #-}

instance CoArbitrary Term
