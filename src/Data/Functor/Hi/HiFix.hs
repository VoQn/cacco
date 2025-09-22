{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Hi.HiFix where

--
import Data.Functor.Classes
import Data.Kind (Type)
import Data.Typeable (Typeable)

--
import Data.Functor.Hi.HiFunctor
import Data.Functor.Hi.HiRecursive
import Data.Functor.Hi.Types

-------------------------------------------------------------------------------
-- Higher-order Fix
-------------------------------------------------------------------------------

newtype HiFix (h :: (Type -> Type) -> Type -> Type) a = HiFix (h (HiFix h) a)
    deriving (Typeable)

hunfix :: HiFix h ~> h (HiFix h)
hunfix (HiFix f) = f
{-# INLINE hunfix #-}

-- | 'Eq' instance for 'HiFix'
instance (Eq1 (h (HiFix h)), Eq a) => Eq (HiFix h a) where
    (==) :: (Eq1 (h (HiFix h)), Eq a) => HiFix h a -> HiFix h a -> Bool
    (HiFix f) == (HiFix g) = eq1 f g

-- | 'Ord' instance for 'HiFix'
instance (Ord1 (h (HiFix h)), Ord a) => Ord (HiFix h a) where
    compare :: (Ord1 (h (HiFix h)), Ord a) => HiFix h a -> HiFix h a -> Ordering
    (HiFix f) `compare` (HiFix g) = f `compare1` g

-- | 'Show' instance for 'HiFix'
instance (Show1 (h (HiFix h)), Show a) => Show (HiFix h a) where
    showsPrec :: (Show1 (h (HiFix h)), Show a) => Int -> HiFix h a -> ShowS
    showsPrec n (HiFix f) = showsPrec1 n f

-- | 'Functor' instance for 'HiFix'
deriving instance (Functor (h (HiFix h))) => Functor (HiFix h)

deriving instance (Foldable (h (HiFix h))) => Foldable (HiFix h)
deriving instance (Traversable (h (HiFix h))) => Traversable (HiFix h)

type instance HiBase (HiFix h) = h

instance (HiFunctor f) => HiRecursive (HiFix f) where
    hproject :: (HiFunctor f) => HiFix f ~> HiBase (HiFix f) (HiFix f)
    hproject = hunfix

instance (HiFunctor f) => HiCorecursive (HiFix f) where
    hembed :: (HiFunctor f) => HiBase (HiFix f) (HiFix f) ~> HiFix f
    hembed = HiFix
