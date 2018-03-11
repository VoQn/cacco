{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cacco.Syntax.Literal
( Literal(..)
) where

import           Control.Applicative       (empty, (<|>))
import           Control.DeepSeq           (NFData)
import           Data.Data                 (Data)
import           Data.Int
import           Data.Word
import           Data.Scientific           (Scientific, fromFloatDigits,
                                            scientific)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Pretty (..), dquotes, (<>))
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Numeric.Natural           (Natural)

import           Test.SmallCheck.Series

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

instance Pretty Literal where
    pretty literal = case literal of
        Undef     -> "undefined"
        Unit      -> "()"
        Bool True -> "true"
        Bool ____ -> "false"
        Int8    x -> pretty x <> "_i8"
        Int16   x -> pretty x <> "_i16"
        Int32   x -> pretty x <> "_i32"
        Int64   x -> pretty x <> "_i64"
        Uint8   x -> pretty x <> "_u8"
        Uint16  x -> pretty x <> "_u16"
        Uint32  x -> pretty x <> "_u32"
        Uint64  x -> pretty x <> "_u64"
        Integer x -> (if x < 0 then "" else "+") <> pretty x
        Natural x -> pretty x
        Float16 x -> pretty (show x) <> "_f16"
        Float32 x -> pretty (show x) <> "_f32"
        Float64 x -> pretty (show x) <> "_f64"
        Flonum  x -> pretty (show x)
        Text    x -> dquotes $ pretty x

ints :: (Monad m, Integral n, Bounded n) => Series m n
ints = generate zero <|> nats \/ (negate <$> nats)
  where
    zero d | d >= 0 = pure 0 | otherwise = empty
    nats = generate $ \d -> take d [1..maxBound]

uints :: (Monad m, Integral n, Bounded n) => Series m n
uints = generate $ \d -> take (d + 1) [0..maxBound]

instance Monad m => Serial m Int8  where series = ints
instance Monad m => Serial m Int16 where series = ints
instance Monad m => Serial m Int32 where series = ints
instance Monad m => Serial m Int64 where series = ints

instance Monad m => Serial m Word8  where series = uints
instance Monad m => Serial m Word16 where series = uints
instance Monad m => Serial m Word32 where series = uints
instance Monad m => Serial m Word64 where series = uints

instance Monad m => Serial m Literal where
    series =
        cons0 Undef \/
        cons0 Unit \/
        cons1 Bool \/
        cons1 (Int8  . (fromIntegral :: Int8  -> Integer)) \/
        cons1 (Int16 . (fromIntegral :: Int16 -> Integer)) \/
        cons1 (Int32 . (fromIntegral :: Int32 -> Integer)) \/
        cons1 (Int64 . (fromIntegral :: Int64 -> Integer)) \/
        cons1 Integer \/
        cons1 (Uint8  . (fromIntegral :: Word8  -> Natural)) \/
        cons1 (Uint16 . (fromIntegral :: Word16 -> Natural)) \/
        cons1 (Uint32 . (fromIntegral :: Word32 -> Natural)) \/
        cons1 (Uint64 . (fromIntegral :: Word64 -> Natural)) \/
        cons1 Natural \/
        cons1 (Float16 . (fromFloatDigits :: Float -> Scientific)) \/
        cons1 (Float32 . (fromFloatDigits :: Float -> Scientific)) \/
        cons1 (Float64 . (fromFloatDigits :: Double -> Scientific)) \/
        (Flonum  <$> cons2 scientific) \/
        cons1 (Text . T.pack)
