{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cacco.Ann where

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import           Cacco.Fix     (Fix (..), cata)

-- | Annotated Functor
newtype AnnF i f a = AnnF { unAnnF :: (i, f a) }
  deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable)

-- | 'traverse' does not changed annotation
instance Traversable t => Traversable (AnnF i t) where
  traverse f (AnnF (i, t)) = AnnF . (,) i <$> traverse f t

-- | Fixed annotated functor
type Ann i f = Fix (AnnF i f)

-- | Add some annotation to 'Fix' functor.
add :: Functor f
    => i       -- ^ Additional information
    -> Fix f   -- ^ Fixed contents functor
    -> Ann i f
add i = cata $ Fix . AnnF . (,) i

-- | Get annotation from 'Ann'
info :: Functor f => Ann i f -> i
info = cata $ fst . unAnnF

-- | Remove annotation from 'Ann'
remove :: Functor f => Ann i f -> Fix f
remove = cata $ Fix . snd . unAnnF
