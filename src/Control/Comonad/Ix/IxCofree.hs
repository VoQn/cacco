{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Comonad.Ix.IxCofree where

import Data.Functor.Ix.IxFunctor
import Data.Functor.Ix.IxRecursive
import Data.Functor.Ix.Types

-------------------------------------------------------------------------------
-- Indexed Cofree
-------------------------------------------------------------------------------

-- | Indexed Cofree
data IxCofree f a i = a i :< f (IxCofree f a) i

-- | Base Functor for Indexed Cofree
data IxCofreeF f a b i = a i :<< f b i

type instance IxBase (IxCofree f a) = IxCofreeF f a

instance (IxFunctor f) => IxFunctor (IxCofreeF f a) where
    imap f (x :<< xs) = x :<< imap f xs

instance (IxFunctor f) => IxRecursive (IxCofree f a) where
    iproject (x :< xs) = x :<< xs

instance (IxFunctor f) => IxCorecursive (IxCofree f a) where
    iembed (x :<< xs) = x :< xs

icofree :: IxCofreeF h f (IxCofree h f) ~> IxCofree h f
icofree (x :<< xs) = x :< xs

icoiter :: (IxFunctor f) => IxCoalgebra f a -> a ~> IxCofree f a
icoiter f a = a :< (icoiter f `imap` f a)
