{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Fix where

import           Control.Arrow
import           Control.Monad
import           Data.Function ((&))
import           Data.Typeable
import           GHC.Generics

newtype Fix f = Fix { out :: f (Fix f) }

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Typeable (f (Fix f)) => Typeable (Fix f)
deriving instance Generic (f (Fix f)) => Generic (Fix f)

type Cofunctor f a = f a -> a
type Coalgebra f a = a -> f a
type RAlgebra f a = f (Fix f, a) -> a
type RCoalgebra f a = f (Either (Fix f) a)

-- | paramorphism folding
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = out >>> fmap (id &&& para f) >>> f
{-# INLINEABLE para #-}

para' :: Functor f => (Fix f -> f a -> a) -> Fix f -> a
para' alg t = out t & fmap (para' alg) & alg t
{-# INLINEABLE para' #-}

-- | apomorphism unforlding
apo :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo f = f >>> fmap (id ||| apo f) >>> Fix

-- | catamorphism folding.
cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata phi = out >>> fmap (cata phi) >>> phi

-- | monadic catamorphism folding.
cataM :: (Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
cataM phi = out >>> traverse (cataM phi) >=> phi

-- | anamorphism unfolding.
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = psi >>> fmap (ana psi) >>> Fix

anaM :: (Monad m, Traversable f) => (a -> m (f a)) -> a -> m (Fix f)
anaM psi = psi >=> traverse (anaM psi) >>> fmap Fix

-- | hylomorhism folding
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = ana g >>> cata f

-- | metamorphism unfolding
meta :: (Functor f, Functor g) => (f a -> a) -> (a -> g a) -> Fix f -> Fix g
meta f g = cata f >>> ana g
