{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Functor.Ix.IxFree where
--
import           Data.Functor.Classes
import           Data.Typeable

import           Data.Functor.Ix.IxEq
import           Data.Functor.Ix.IxFoldable
import           Data.Functor.Ix.IxFunctor
import           Data.Functor.Ix.IxRecursive
import           Data.Functor.Ix.IxTraversable
import           Data.Functor.Ix.Types

-------------------------------------------------------------------------------
-- Indexed Ideal
-------------------------------------------------------------------------------
data IxIdeal f a i = IxP (a i) | IxI (f a i)

instance (IxEq a, IxEq (f a)) => IxEq (IxIdeal f a) where
    ieq (IxP a) (IxP b) = ieq a b
    ieq (IxI f) (IxI g) = ieq f g
    ieq _ _             = False

instance (Eq1 a, Eq1 (f a), Eq i) => Eq (IxIdeal f a i) where
    IxP a == IxP b = a `eq1` b
    IxI a == IxI b = a `eq1` b
    _ == _ = False

instance (Show1 a, Show1 (f a), Show i) => Show (IxIdeal f a i) where
    showsPrec n (IxP a) = showsPrec1 n a
    showsPrec n (IxI a) = showsPrec1 n a

deriving instance (Functor a, Functor (f a)) => Functor (IxIdeal f a)

instance IxFunctor f => IxFunctor (IxIdeal f) where
    imap f (IxP a)  = IxP (f a)
    imap f (IxI fa) = IxI $ f `imap` fa

class IxFunctor f => Mu' f where
    mu' :: f (IxIdeal f a) ~> f a

instance IxFunctor f => IxApplicative (IxIdeal f) where
    ireturn = IxP

instance Mu' f => IxMonad (IxIdeal f) where
    ibind f (IxP a)  = f a
    ibind f (IxI fa) = IxI $ mu' $ f `imap` fa

-------------------------------------------------------------------------------
-- Indexed Free
-------------------------------------------------------------------------------
class IxMonad m => IxMonadFree f m | m -> f where
    ifree :: f (m a) ~> m a

-- | Indexed Free
data IxFree f a i where
    IxPure :: a i -> IxFree f a i
    IxFree :: f (IxFree f a) i -> IxFree f a i

deriving instance Typeable (IxFree f a)

instance (Eq i, Eq1 a, Eq1 (f (IxFree f a))) => Eq (IxFree f a i) where
    IxPure a == IxPure b = a `eq1` b
    IxFree f == IxFree g = f `eq1` g
    _ == _ = False

deriving instance
    (Show (f i), Show (h (IxFree h f) i))
    => Show (IxFree h f i)

deriving instance
    (Functor f, Functor (h (IxFree h f)))
    => Functor (IxFree h f)

deriving instance
    (Foldable a, Foldable (f (IxFree f a)))
    => Foldable (IxFree f a)

deriving instance
    (Traversable a, Traversable (f (IxFree f a)))
    => Traversable (IxFree f a)

instance IxFunctor f => IxFunctor (IxFree f) where
    imap f (IxPure a) = IxPure $ f a
    imap f (IxFree w) = IxFree $ imap (imap f) w

instance IxFoldable f => IxFoldable (IxFree f) where
    ifoldMap f (IxPure a) = f a
    ifoldMap f (IxFree w) = ifoldMap (ifoldMap f) w

instance IxTraversable f => IxTraversable (IxFree f) where
    itraverse f (IxPure a) = IxPure <$> f a
    itraverse f (IxFree w) = IxFree <$> itraverse (itraverse f) w

instance IxFunctor m => IxApplicative (IxFree m) where
    ireturn = IxPure

instance IxFunctor m => IxMonad (IxFree m) where
    ibind f (IxPure a) = f a
    ibind f (IxFree g) = IxFree $ ibind f `imap` g

instance IxFunctor f => IxMonadFree f (IxFree f) where
    ifree = IxFree

-------------------------------------------------------------------------------
-- Indexed Free-Functor
-------------------------------------------------------------------------------

-- | Base Functor for Indexed Free
data IxFreeF f a b i where
    IxPureF :: a i -> IxFreeF f a b i
    IxFreeF :: f b i -> IxFreeF f a b i

type instance IxBase (IxFree f a) = (IxFreeF f a)

instance IxFunctor f => IxFunctor (IxFreeF f a) where
    imap _ (IxPureF a) = IxPureF a
    imap f (IxFreeF g) = IxFreeF $ imap f g

instance IxFunctor f => IxRecursive (IxFree f a) where
    iproject (IxPure f) = IxPureF f
    iproject (IxFree f) = IxFreeF f

instance IxFunctor f => IxCorecursive (IxFree f a) where
    iembed (IxPureF f) = IxPure f
    iembed (IxFreeF f) = IxFree f

-------------------------------------------------------------------------------
-- Indexed Cofree
-------------------------------------------------------------------------------

-- | Indexed Cofree
data IxCofree h f i = (f i) :< (h (IxCofree h f) i)

-- | Base Functor for Indexed Cofree
data IxCofreeF h f g i = (f i) :<< (h g i)

type instance IxBase (IxCofree h f) = IxCofreeF h f

instance IxFunctor f => IxFunctor (IxCofreeF f a) where
    imap f (x :<< xs) = x :<< imap f xs

instance IxFunctor f => IxRecursive (IxCofree f a) where
    iproject (x :< xs) = x :<< xs

instance IxFunctor f => IxCorecursive (IxCofree f a) where
    iembed (x :<< xs) = x :< xs

icofree :: IxCofreeF h f (IxCofree h f) ~> IxCofree h f
icofree (x :<< xs) = x :< xs
