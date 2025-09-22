{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Ix.IxFoldable where

--
import Data.Functor.Ix.Types

class IxFoldable t where
    ifoldMap :: (Monoid m) => f ~>. m -> t f ~>. m
