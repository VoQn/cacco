{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}

module Data.IxAnn where

import           Data.Typeable

import           Data.IxFix

newtype IxAnnF a t f i = IxAnnF { unIxAnnF :: (a, t f i) }
  deriving (Eq, Ord, Show, Typeable)

instance IxTraversable t => IxFunctor (IxAnnF a t) where
  imap = imapDefault

instance IxTraversable t => IxFoldable (IxAnnF a t) where
  ifoldMap = ifoldMapDefault

instance IxTraversable t => IxTraversable (IxAnnF a t) where
  itraverse f (IxAnnF (i, t)) = IxAnnF . (,) i <$> itraverse f t

type IxAnn a t i = IxFix (IxAnnF a t) i

--
addAnn :: IxTraversable t => a -> IxFix t i -> IxAnn a t i
addAnn ann = cata $ In . IxAnnF . (,) ann

getAnn :: IxTraversable t => IxAnn a t i -> a
getAnn = cata' $ fst . unIxAnnF

removeAnn :: IxTraversable t => IxAnn a t i -> IxFix t i
removeAnn = cata $ In . snd . unIxAnnF
