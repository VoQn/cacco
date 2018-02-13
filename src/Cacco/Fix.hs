{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Cacco.Fix where

import           Control.Monad

newtype Fix f = Fix { out :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

fold :: Functor f
  => (f a -> a)
  -> Fix f
  -> a
fold acc (Fix f) = acc (fold acc <$> f)

-- | catamorphism folding.
cata :: Functor f
     => (f a -> a)
     -> (Fix f -> a)
cata phi = phi . fmap (cata phi) . out

-- | monadic catamorphism folding.
cataM :: (Monad m, Traversable t)
      => (t a -> m a)
      -> (Fix t -> m a)
cataM phi = phi <=< traverse (cataM phi) . out
