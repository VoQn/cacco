{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Hi.HiFree where

--
import Data.Functor.Classes
import Data.Typeable (Typeable)

--
import Data.Functor.Hi.HiFunctor
import Data.Functor.Hi.HiRecursive
import Data.Functor.Hi.Types
import Data.Kind (Type)

-------------------------------------------------------------------------------
-- Higher-order Free
-------------------------------------------------------------------------------

class (HiMonad m) => HiMonadFree h m | m -> h where
    hfree :: h (m a) ~> m a

-- | Higher-order version of the 'Free'
data HiFree (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) (a :: Type) where
    HiPure :: f a -> HiFree h f a
    HiFree :: h (HiFree h f) a -> HiFree h f a

instance (Eq1 f, Eq1 (h (HiFree h f)), Eq a) => Eq (HiFree h f a) where
    HiPure a == HiPure b = a `eq1` b
    HiFree as == HiFree bs = as `eq1` bs
    _ == _ = False

instance (Show1 f, Show1 (h (HiFree h f)), Show a) => Show (HiFree h f a) where
    showsPrec n (HiPure f) = showsPrec1 n f
    showsPrec n (HiFree f) = showsPrec1 n f

deriving instance Typeable (HiFree h f)

deriving instance
    ( Functor f
    , Functor (h (HiFree h f))
    ) =>
    Functor (HiFree h f)

deriving instance
    ( Foldable f
    , Foldable (h (HiFree h f))
    ) =>
    Foldable (HiFree h f)

deriving instance
    ( Traversable f
    , Traversable (h (HiFree h f))
    ) =>
    Traversable (HiFree h f)

instance (HiFunctor h) => HiFunctor (HiFree h) where
    hmap f (HiPure a) = HiPure $ f a
    hmap f (HiFree as) = HiFree $ hmap (hmap f) as

instance (HiFunctor h) => HiApplicative (HiFree h) where
    hreturn = HiPure

instance (HiFunctor h) => HiMonad (HiFree h) where
    hbind f (HiPure a) = f a
    hbind f (HiFree as) = HiFree $ hbind f `hmap` as

instance (HiFunctor h) => HiMonadFree h (HiFree h) where
    hfree = HiFree

-- | Higher-order Base Functor of 'HiFree'
data HiFreeF (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) (g :: Type -> Type) (a :: Type) where
    HiPureF :: f a -> HiFreeF h f g a
    HiFreeF :: h g a -> HiFreeF h f g a

instance (Eq1 f, Eq1 (h g), Eq a) => Eq (HiFreeF h f g a) where
    HiPureF a == HiPureF b = a `eq1` b
    HiFreeF as == HiFreeF bs = as `eq1` bs
    _ == _ = False

instance (Show1 f, Show1 (h g), Show a) => Show (HiFreeF h f g a) where
    showsPrec n (HiPureF a) = showsPrec1 n a
    showsPrec n (HiFreeF as) = showsPrec1 n as

deriving instance Typeable (HiFreeF h f g)

type instance HiBase (HiFree h f) = HiFreeF h f

instance (HiFunctor h) => HiFunctor (HiFreeF h f) where
    hmap f fr = case fr of
        HiPureF a -> HiPureF a
        HiFreeF as -> HiFreeF $ hmap f as

instance (HiFunctor h) => HiRecursive (HiFree h f) where
    hproject fr = case fr of
        HiPure a -> HiPureF a
        HiFree as -> HiFreeF as

instance (HiFunctor h) => HiCorecursive (HiFree h f) where
    hembed fr = case fr of
        HiPureF a -> HiPure a
        HiFreeF as -> HiFree as

-------------------------------------------------------------------------------
-- Higher-order Free
-------------------------------------------------------------------------------

-- | Higher-order Base Functor of 'HiCofree'
data HiCofreeF (h :: (Type -> Type) -> Type -> Type) (p :: Type -> Type) (f :: Type -> Type) (a :: Type)
    = p a :<< h f a

-- | Higher-order version of the 'Cofree'
data HiCofree (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) (a :: Type)
    = f a :< h (HiCofree h f) a

type instance HiBase (HiCofree h f) = HiCofreeF h f

instance (HiFunctor h) => HiFunctor (HiCofreeF h p) where
    hmap f (x :<< xs) = x :<< hmap f xs

instance (HiFunctor h) => HiRecursive (HiCofree h f) where
    hproject (x :< xs) = x :<< xs

instance (HiFunctor h) => HiCorecursive (HiCofree h f) where
    hembed (x :<< xs) = x :< xs

type HiCVAlgebra h f a = f (HiCofree h f a) -> a
