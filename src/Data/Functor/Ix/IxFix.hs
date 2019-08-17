{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Ix.IxFix where
--
import           Data.Function                  ( on )
import           Data.Typeable

import           Data.Functor.Ix.IxEq
import           Data.Functor.Ix.IxFunctor
import           Data.Functor.Ix.IxRecursive
import           Data.Functor.Ix.Types

-------------------------------------------------------------------------------
-- Indexed Fix
-------------------------------------------------------------------------------

-- | Indexed Fix
newtype IxFix (f :: (k -> *) -> k -> *) (i :: k) = IxFix (f (IxFix f) i)

type instance IxBase (IxFix f) = f

-- | unfix for 'IxFix' version
iunfix :: IxFix f ~> f (IxFix f)
iunfix (IxFix f) = f

deriving instance Eq (f (IxFix f) i) => Eq (IxFix f i)
deriving instance Ord (f (IxFix f) i) => Ord (IxFix f i)
deriving instance Show (f (IxFix f) i) => Show (IxFix f i)
deriving instance Typeable (IxFix f)
deriving instance Functor (f (IxFix f)) => Functor (IxFix f)
deriving instance Foldable (f (IxFix f)) => Foldable (IxFix f)
deriving instance Traversable (f (IxFix f)) => Traversable (IxFix f)

instance IxFunctor f => IxRecursive (IxFix f) where
  iproject (IxFix a) = a

instance IxFunctor f => IxCorecursive (IxFix f) where
  iembed = IxFix

irefix :: (IxRecursive s, IxCorecursive t, IxBase s ~ IxBase t) => s ~> t
irefix = icata iembed

toIxFix :: IxRecursive t => t ~> IxFix (IxBase t)
toIxFix = irefix

fromIxFix :: IxCorecursive t => IxFix (IxBase t) ~> t
fromIxFix = irefix

instance IxEq (f (IxFix f)) => IxEq (IxFix f) where
  ieq = ieq `on` iunfix
