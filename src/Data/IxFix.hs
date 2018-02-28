{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.IxFix where

import           Control.Monad
import           Data.Functor.Const
import           Data.Functor.Identity

-- | Natural transformation
type f ~> g = forall i. f i -> g i
infixr 5 ~>

-- | Constant on the left
type f .~> g = forall i. f -> g i
infixr 5 .~>

-- | Constant on the right
type f ~>. g = forall i. f i -> g
infixr 5 ~>.

type Lim (f :: i -> *) = forall (x :: i). f x

-- | Indexed functor
class IxFunctor (f :: (k -> *) -> (k -> *)) where
  -- | map with taking over each index
  imap :: a ~> b -> f a ~> f b

  (/$/) :: (a ~> b) -> f a ~> f b
  (/$/) = imap

  (/$) :: Lim b -> f a ~> f b
  b /$ f = imap (const b) f

-- | Indexed traversable
class IxFunctor t => IxTraversable t where
  -- | traverse with taking over each index
  itraverse :: Applicative f
            => (forall i. a i -> f (b i))     -- ^ lifting functor
            -> (forall j. t a j -> f (t b j))

-- | Indexed foldable
class IxFoldable (f :: (x -> *) -> y -> *) where
  ifoldMap :: forall (a :: x -> *) (m :: *). Monoid m => a ~>. m -> f a ~>. m

data IxFree f a i where
  Pure :: a i -> IxFree f a i
  Free :: f (IxFree f a) i -> IxFree f a i

instance IxFunctor f => IxFunctor (IxFree f) where
  imap f (Pure a) = Pure (f a)
  imap f (Free w) = Free (imap (imap f) w)

imapDefault :: IxTraversable t  => (a ~> b) -> (t a ~> t b)
imapDefault f = runIdentity . itraverse (Identity . f)

ifoldMapDefault :: (IxTraversable t, Monoid m) => (a ~>. m) -> (t a ~>. m)
ifoldMapDefault f = getConst . itraverse (Const . f)

-- | Indexed fix-point
newtype IxFix f i = In { out :: f (IxFix f) i }

deriving instance Show (f (IxFix f) t) => Show (IxFix f t)
deriving instance Eq (f (IxFix f) t) => Eq (IxFix f t)
deriving instance Ord (f (IxFix f) t) => Ord (IxFix f t)
deriving instance Functor (f (IxFix f)) => Functor (IxFix f)

type IxAlgebra f a = f a ~> a
type IxCoalgebra f a = a ~> f a

-- | folding returning indexed type.
cata :: IxFunctor f => IxAlgebra f a -> IxFix f ~> a
cata phi = phi . imap (cata phi) . out

-- | monadic folding returning indexed type.
cataM :: (Monad m, IxTraversable t)
      => (forall i. t a i -> m (a i))
      -> (forall j. IxFix t j -> m (a j))
cataM phi = phi <=< itraverse (cataM phi) . out

-- | folding returning constant type.
cata' :: IxFunctor f => (f (Const a) ~>. a) -> (IxFix f ~>. a)
cata' phi = getConst . cata (Const . phi)

-- | monadic folding returning constant type.
cataM' :: (Monad m, IxTraversable t)
       => (t (Const a) ~>. m a)
       -> (IxFix t ~>. m a)
cataM' phi = phi <=< itraverse (fmap Const . cataM' phi) . out

ana :: IxFunctor f => IxCoalgebra f a -> a ~> IxFix f
ana psi = In . imap (ana psi) . psi

anaM :: (Monad m, IxTraversable t)
     => (forall i. a i -> m (t a i))
     -> (forall j. a j -> m (IxFix t j))
anaM psi = fmap In . (itraverse (anaM psi) <=< psi)

hylo :: IxFunctor f => IxAlgebra f b -> IxCoalgebra f a -> a ~> b
hylo f g = cata f . ana g
