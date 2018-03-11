{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Test.SmallCheck.Numeric () where

import           Control.Applicative
import           Data.Int
import           Data.Word
import           Test.SmallCheck.Series

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
