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

instance Traversable t => Traversable (AnnF i t) where
  traverse f (AnnF (i, t)) = AnnF . (,) i <$> traverse f t

type Ann i f = Fix (AnnF i f)

add :: Functor f => i -> Fix f -> Ann i f
add i = cata $ Fix . AnnF . (,) i

info :: Functor f => Ann i f -> i
info = cata $ fst . unAnnF

remove :: Functor f => Ann i f -> Fix f
remove = cata $ Fix . snd . unAnnF
