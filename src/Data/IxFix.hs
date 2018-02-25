{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.IxFix where

import           Control.Monad

-- | Natural transformation
type f ~> g = forall i. f i -> g i
infixr 5 ~>

-- | Constant on the left
type f .~> g = forall i. f -> g i
infixr 5 .~>

-- | Constant on the right
type f ~>. g = forall i. f i -> g
infixr 5 ~>.

-- | synonym type 'K'onst functor
newtype K a b = K { unK:: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | synonym type 'I'dentity functor
newtype I a = I { unI :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative I where
  pure = I
  I f <*> I a = I (f a)

instance Monoid a => Applicative (K a) where
  pure _ = K mempty
  K a <*> K a' = K (mappend a a')

-- | Indexed functor
class IxFunctor (f :: (k -> *) -> (k -> *)) where
  -- | map with taking over each index
  imap :: (a ~> b)     -- ^ convert function switch functor wrap
       -> (f a ~> f b)

-- | Indexed traversable
class IxFunctor t => IxTraversable t where
  -- | traverse with taking over each index
  itraverse :: Applicative f
            => (forall i. a i -> f (b i))     -- ^ lifting functor
            -> (forall i. t a i -> f (t b i))

-- | Indexed foldable
class IxFoldable t where
  iFoldMap :: Monoid m
           => (a ~>. m)
           -> (t a ~>. m)

imapDefault :: IxTraversable t
            => (a ~> b)
            -> (t a ~> t b)
imapDefault f = unI . itraverse (I . f)

iFoldMapDefault :: (IxTraversable t, Monoid m)
                => (a ~>. m)
                -> (t a ~>. m)
iFoldMapDefault f = unK . itraverse (K . f)

-- | Indexed fix-point
newtype IxFix f i = In { out :: f (IxFix f) i }

deriving instance Show (f (IxFix f) t) => Show (IxFix f t)
deriving instance Eq (f (IxFix f) t) => Eq (IxFix f t)
deriving instance Ord (f (IxFix f) t) => Ord (IxFix f t)

-- | folding returning indexed type.
cata :: IxFunctor f
     => (f a ~> a)
     -> (IxFix f ~> a)
cata phi = phi . imap (cata phi) . out

-- | monadic folding returning indexed type.
cataM :: (Monad m, IxTraversable t)
      => (forall i. t a i -> m (a i))
      -> (forall i. IxFix t i -> m (a i))
cataM phi = phi <=< itraverse (cataM phi) . out

-- | folding returning constant type.
cata' :: IxFunctor f
      => (f (K a) ~>. a)
      -> (IxFix f ~>. a)
cata' phi = unK . cata (K . phi)

-- | monadic folding returning constant type.
cataM' :: (Monad m, IxTraversable t)
       => (t (K a) ~>. m a)
       -> (IxFix t ~>. m a)
cataM' phi = phi <=< itraverse (fmap K . cataM' phi) . out

