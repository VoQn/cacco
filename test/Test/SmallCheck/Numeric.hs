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
