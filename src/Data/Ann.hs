{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Ann where

import Control.Arrow
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata)

-- | Annotated Functor
newtype AnnF i f a = AnnF {unAnnF :: (f a, i)}
    deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable)

-- | 'traverse' does not changed annotation
instance (Traversable t) => Traversable (AnnF i t) where
    traverse ::
        (Traversable t, Applicative f) =>
        (a -> f b) ->
        AnnF i t a ->
        f (AnnF i t b)
    traverse f (AnnF (t, i)) = AnnF . (id &&& const i) <$> traverse f t

-- | Fixed annotated functor
type Ann i f = Fix (AnnF i f)

-- | Add some annotation to 'Fix' functor.
add ::
    (Functor f) =>
    -- | Additional information
    i ->
    -- | Fixed contents functor
    Fix f ->
    Ann i f
add i = cata $ Fix . AnnF . (id &&& const i)

-- | Get annotation from 'Ann'
info :: (Functor f) => Ann i f -> i
info = cata $ snd . unAnnF

-- | Remove annotation from 'Ann'
remove :: (Functor f) => Ann i f -> Fix f
remove = cata $ Fix . fst . unAnnF
