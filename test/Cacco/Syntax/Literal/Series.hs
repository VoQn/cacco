{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cacco.Syntax.Literal.Series where

import           Data.Int
import           Data.Scientific
import qualified Data.Text               as T
import           Data.Word
import           Numeric.Natural
import           Test.SmallCheck.Series

import           Cacco.Syntax.Literal
import           Test.SmallCheck.Numeric ()

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
