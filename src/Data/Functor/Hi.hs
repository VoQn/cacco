{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Functor.Hi where

import           Control.Arrow             ((>>>))
import           Data.Functor.Classes
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Typeable             (Typeable)

import           Data.Functor.Hi.HiFunctor
import           Data.Functor.Hi.Types

-------------------------------------------------------------------------------
-- Higher-order Traversable
-------------------------------------------------------------------------------

class HiTraversable (h :: (* -> *) -> * -> *) where
    htraverse :: Applicative m => NatM m f g -> NatM m (h f) (h g)
    hmapM :: Monad m => NatM m f g -> NatM m (h f) (h g)

hmapDefault :: HiTraversable h => (f ~> g) -> (h f ~> h g)
hmapDefault f = htraverse (f >>> Identity) >>> runIdentity
{-# INLINE hmapDefault #-}

hfoldMapDefault :: (HiTraversable h, Monoid m) => (a ~>. m) -> (h a ~>. m)
hfoldMapDefault f = htraverse (f >>> Const) >>> getConst
{-# INLINE hfoldMapDefault #-}

-------------------------------------------------------------------------------
-- Higher-order Recursive
-------------------------------------------------------------------------------

-- | Higher-order version of the 'Recursive' type-class
class HiFunctor (HiBase f) => HiRecursive (f :: * -> *) where
    hproject :: f ~> (HiBase f) f

    -- | catamorphism folding for 'HiRecursive'
    hcata :: HiAlgebra (HiBase f) g -> f ~> g
    hcata phi = hproject >>> hmap (hcata phi) >>> phi

    -- | catamorphism folding returns constant value
    kcata :: KAlgebra (HiBase f) a -> f ~>. a
    kcata phi = hcata (phi >>> Const) >>> getConst

    hpara :: HiRAlgebra f g -> f ~> g
    hpara phi = hproject >>> hmap (hpara phi &&&& id) >>> phi

-- | Higher-order version of the 'CoRecursive' type-class
class HiFunctor (HiBase f) => HiCorecursive (f :: * -> *) where
    hembed :: (HiBase f) f ~> f

    -- | anamorphism unfolding
    hana :: HiCoalgebra g (HiBase f) -> g ~> f
    hana psi = psi >>> hmap (hana psi) >>> hembed

    -- | apomorphism unfolding
    hapo :: HiRCoalgebra g f -> g ~> f
    hapo psi = psi >>> hmap (id |||| hapo psi) >>> hembed

-------------------------------------------------------------------------------
-- Higher-order Free & Cofree
-------------------------------------------------------------------------------

newtype HiFix (h :: (* -> *) -> * -> *) a = HiFix (h (HiFix h) a)
    deriving (Typeable)

hunfix :: HiFix h ~> h (HiFix h)
hunfix (HiFix f) = f
{-# INLINE hunfix #-}

instance (Eq1 (h (HiFix h)), Eq a) => Eq (HiFix h a) where
    (HiFix f) == (HiFix g) = eq1 f g

instance (Ord1 (h (HiFix h)), Ord a) => Ord (HiFix h a) where
    (HiFix f) `compare` (HiFix g) = f `compare1` g

instance (Show1 (h (HiFix h)), Show a) => Show (HiFix h a) where
    showsPrec n (HiFix f) = showsPrec1 n f

deriving instance Functor (h (HiFix h)) => Functor (HiFix h)
deriving instance Foldable (h (HiFix h)) => Foldable (HiFix h)
deriving instance Traversable (h (HiFix h)) => Traversable (HiFix h)

type instance HiBase (HiFix h) = h

instance HiFunctor f => HiRecursive (HiFix f) where
    hproject = hunfix

instance HiFunctor f => HiCorecursive (HiFix f) where
    hembed = HiFix

-------------------------------------------------------------------------------
-- Higher-order Free
-------------------------------------------------------------------------------

class HiMonad m => HiMonadFree h m | m -> h where
    hfree :: h (m a) ~> m a

-- | Higher-order version of the 'Free'
data HiFree (h :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) where
    HiPure :: f a -> HiFree h f a
    HiFree :: h (HiFree h f) a -> HiFree h f a

deriving instance Typeable (HiFree h f)

deriving instance
    ( Functor f
    , Functor (h (HiFree h f))
    ) => Functor (HiFree h f)

deriving instance
    ( Foldable f
    , Foldable (h (HiFree h f))
    ) => Foldable (HiFree h f)

deriving instance
    ( Traversable f
    , Traversable (h (HiFree h f))
    ) => Traversable (HiFree h f)

instance HiFunctor h => HiFunctor (HiFree h) where
    hmap f fr = case fr of
        HiPure a  -> HiPure $ f a
        HiFree as -> HiFree $ hmap (hmap f) as

instance HiFunctor h => HiApplicative (HiFree h) where
    hreturn = HiPure

instance HiFunctor h => HiMonad (HiFree h) where
    hbind f fr = case fr of
        HiPure a  -> f a
        HiFree as -> HiFree $ hbind f `hmap` as

instance HiFunctor h => HiMonadFree h (HiFree h) where
    hfree = HiFree

-- | Higher-order Base Functor of 'HiFree'
data HiFreeF (h :: (* -> *) -> * -> *) (f :: * -> *) (g :: * -> *) (a :: *)
  where
    HiPureF :: f a   -> HiFreeF h f g a
    HiFreeF :: h g a -> HiFreeF h f g a

instance (Eq1 f, Eq1 (h g), Eq a) => Eq (HiFreeF h f g a) where
    (==) x y = case (x, y) of
        (HiPureF a,  HiPureF b)  -> a `eq1` b
        (HiFreeF as, HiFreeF bs) -> as `eq1` bs
        _                        -> False

instance (Show1 f, Show1 (h g), Show a) => Show (HiFreeF h f g a) where
    showsPrec n (HiPureF a)  = showsPrec1 n a
    showsPrec n (HiFreeF as) = showsPrec1 n as

deriving instance Typeable (HiFreeF h f g)

type instance HiBase (HiFree h f) = HiFreeF h f

instance HiFunctor h => HiFunctor (HiFreeF h f) where
    hmap f fr = case fr of
        HiPureF a  -> HiPureF a
        HiFreeF as -> HiFreeF $ hmap f as

instance HiFunctor h => HiRecursive (HiFree h f) where
    hproject fr = case fr of
        HiPure a  -> HiPureF a
        HiFree as -> HiFreeF as

instance HiFunctor h => HiCorecursive (HiFree h f) where
    hembed fr = case fr of
        HiPureF a  -> HiPure a
        HiFreeF as -> HiFree as

-------------------------------------------------------------------------------
-- Higher-order Free
-------------------------------------------------------------------------------
-- | Higher-order Base Functor of 'HiCofree'
data HiCofreeF (h :: (* -> *) -> * -> *) (p :: * -> *) (f :: * -> *) (a :: *)
    = p a :<< h f a

-- | Higher-order version of the 'Cofree'
data HiCofree (h :: (* -> *) -> * -> *) (f :: * -> *) (a :: *)
    = f a :< h (HiCofree h f) a

type instance HiBase (HiCofree h f) = HiCofreeF h f

instance HiFunctor h => HiFunctor (HiCofreeF h p) where
    hmap f (x :<< xs) = x :<< hmap f xs

instance HiFunctor h => HiRecursive (HiCofree h f) where
    hproject (x :< xs) = x :<< xs

instance HiFunctor h => HiCorecursive (HiCofree h f) where
    hembed (x :<< xs) = x :< xs

type HiCVAlgebra h f a = f (HiCofree h f a) -> a
