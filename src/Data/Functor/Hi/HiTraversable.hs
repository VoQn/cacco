{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Hi.HiTraversable where

import Control.Arrow ((>>>))
import Data.Functor.Const (
    Const (..),
    getConst,
 )
import Data.Functor.Identity (
    Identity (..),
    runIdentity,
 )
import Data.Kind (Type)

import Data.Functor.Hi.Types

-------------------------------------------------------------------------------
-- Higher-order Traversable
-------------------------------------------------------------------------------

class HiTraversable (h :: (Type -> Type) -> Type -> Type) where
    htraverse :: (Applicative m) => NatM m f g -> NatM m (h f) (h g)
    hmapM :: (Monad m) => NatM m f g -> NatM m (h f) (h g)

hmapDefault :: (HiTraversable h) => (f ~> g) -> (h f ~> h g)
hmapDefault f = htraverse (f >>> Identity) >>> runIdentity
{-# INLINE hmapDefault #-}

hfoldMapDefault :: (HiTraversable h, Monoid m) => (a ~>. m) -> (h a ~>. m)
hfoldMapDefault f = htraverse (f >>> Const) >>> getConst
{-# INLINE hfoldMapDefault #-}
