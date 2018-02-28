{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Fix where

import           Control.Monad

newtype Fix f = Fix { out :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

type Cofunctor f a = f a -> a
type Coalgebra f a = a -> f a

-- | catamorphism folding.
cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata phi = phi . fmap (cata phi) . out

-- | monadic catamorphism folding.
cataM :: (Monad m, Traversable t) => (t a -> m a) -> (Fix t -> m a)
cataM phi = phi <=< traverse (cataM phi) . out

-- | anamorphism unfolding.
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = Fix . fmap (ana psi) . psi

anaM :: (Monad m, Traversable t) => (a -> m (t a)) -> (a -> m (Fix t))
anaM psi = fmap Fix . (traverse (anaM psi) <=< psi)
