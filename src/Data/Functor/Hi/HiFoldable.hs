{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Hi.HiFoldable where

import Control.Arrow ((>>>))
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Monoid (
    Dual (..),
    Endo (..),
 )

import Data.Functor.Hi.Types (type (~>.))

-------------------------------------------------------------------------------
-- Higher-order Foldable
-------------------------------------------------------------------------------

-- | Higher-order 'Foldable'
class HiFoldable (h :: (Type -> Type) -> Type -> Type) where
    -- | Higher-order version of 'foldMap'
    hfoldMap :: forall m (f :: Type -> Type). (Monoid m) => f ~>. m -> h f ~>. m
    hfoldMap f = hfoldr (f >>> mappend) mempty

    -- | folding constant 'Monoid'
    hfold :: (Monoid m) => h (Const m) ~>. m
    hfold = hfoldMap getConst

    -- | folding according to right hand
    hfoldr ::
        forall (f :: Type -> Type) b.
        () =>
        -- | accumulator
        (f ~>. (b -> b)) ->
        -- | initial value
        b ->
        h f ~>. b
    hfoldr f z t = appEndo endo z
      where
        endo :: Endo b
        endo = hfoldMap (f >>> Endo) t
        {-# INLINE endo #-}

    -- | folding according to left hand
    hfoldl ::
        forall (f :: Type -> Type) b.
        () =>
        -- | accumulator
        (b -> f ~>. b) ->
        -- | initial value
        b ->
        h f ~>. b
    hfoldl f z t = appEndo endo z
      where
        endo :: Endo b
        endo = getDual $ hfoldMap acc t
        {-# INLINE endo #-}

        acc :: f ~>. Dual (Endo b)
        acc = flip f >>> Endo >>> Dual
        {-# INLINE acc #-}

-- | 'foldr' folding Constant values
kfoldr :: (HiFoldable h) => (a -> b -> b) -> b -> h (Const a) ~>. b
kfoldr f = hfoldr (getConst >>> f)

-- | 'foldl' left hand folding Constant values
kfoldl :: forall a b h. (HiFoldable h) => (b -> a -> b) -> b -> h (Const a) ~>. b
kfoldl f = hfoldl acc
  where
    acc :: b -> Const a ~>. b
    acc x = f x . getConst
