{-# LANGUAGE Rank2Types #-}

module Data.Functor.Ix.IxTraversable where

import Data.Functor.Ix.IxFunctor
import Data.Functor.Ix.Types

class (IxFunctor t) => IxTraversable t where
    itraverse :: (Applicative m) => NatM m a b -> NatM m (t a) (t b)
