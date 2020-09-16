{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Hi.HiFix where

--
import           Data.Functor.Classes           ( Eq1
                                                , Ord1
                                                , Show1
                                                , compare1
                                                , eq1
                                                , showsPrec1
                                                )
import           Data.Typeable                  ( Typeable )
--
import           Data.Functor.Hi.HiFunctor      ( HiFunctor )
import           Data.Functor.Hi.HiRecursive    ( HiCorecursive(hembed)
                                                , HiRecursive(hproject)
                                                )
import           Data.Functor.Hi.Types          ( HiBase
                                                , type (~>)
                                                )

-------------------------------------------------------------------------------
-- Higher-order Fix
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
