{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Functor.Ix.IxFree where

import           Control.Arrow
import           Data.Functor.Classes
import           Data.Functor.Const
--
import           Control.Comonad.Ix.IxCofree
import           Control.Monad.Ix.IxFree
--
import           Data.Functor.Ix.IxEq
import           Data.Functor.Ix.IxFix
import           Data.Functor.Ix.IxFunctor
import           Data.Functor.Ix.IxRecursive
import           Data.Functor.Ix.Types

-------------------------------------------------------------------------------
-- Indexed Ideal
-------------------------------------------------------------------------------
data IxIdeal f a i = IxP (a i) | IxI (f a i)

instance (IxEq a, IxEq (f a)) => IxEq (IxIdeal f a) where
  ieq (IxP a) (IxP b) = ieq a b
  ieq (IxI f) (IxI g) = ieq f g
  ieq _       _       = False

instance (Eq1 a, Eq1 (f a), Eq i) => Eq (IxIdeal f a i) where
  IxP a == IxP b = a `eq1` b
  IxI a == IxI b = a `eq1` b
  _     == _     = False

instance (Show1 a, Show1 (f a), Show i) => Show (IxIdeal f a i) where
  showsPrec n (IxP a) = showsPrec1 n a
  showsPrec n (IxI a) = showsPrec1 n a

deriving instance (Functor a, Functor (f a)) => Functor (IxIdeal f a)

instance IxFunctor f => IxFunctor (IxIdeal f) where
  imap f (IxP a ) = IxP (f a)
  imap f (IxI fa) = IxI $ f `imap` fa

class IxFunctor f => Mu' f where
    mu' :: f (IxIdeal f a) ~> f a

instance IxFunctor f => IxApplicative (IxIdeal f) where
  ireturn = IxP

instance Mu' f => IxMonad (IxIdeal f) where
  ibind f (IxP a ) = f a
  ibind f (IxI fa) = IxI $ mu' $ f `imap` fa

-------------------------------------------------------------------------------
-- Cofree Annotation
-------------------------------------------------------------------------------

type AnnF ann t = IxCofreeF t (Const ann)
type Ann ann t = IxFix (IxCofreeF t (Const ann))

rmAnn :: IxCorecursive f => Ann ann (IxBase f) ~> f
rmAnn = icata alg where alg (_ :<< x) = iembed x

-------------------------------------------------------------------------------
-- Yoneda & CoYoneda
-------------------------------------------------------------------------------

newtype IxYoneda (f :: (k -> *) -> k -> *) (a :: k -> *) (i :: k)
    = IxYo { runIxYo :: forall (b :: k -> *). (a ~> b) -> f b i }

iyoneda :: IxFunctor f => f a i -> IxYoneda f a i
iyoneda a = IxYo $ \f -> imap f a

ilowerYo :: IxYoneda f a ~> f a
ilowerYo (IxYo f) = f id

instance IxFunctor (IxYoneda h) where
  imap f m = IxYo $ \k -> runIxYo m (k . f)

data IxCoyoneda (a :: k -> *) (i :: k)
    = forall (b :: k -> *). IxCoyo (b i -> a i) (b i)

icoyo :: a ~> IxCoyoneda a
icoyo = IxCoyo id

lowerCoyo :: IxCoyoneda a ~> a
lowerCoyo (IxCoyo g x) = g x

fromCxt :: (a ~>. b) -> IxCoyoneda a ~>. b
fromCxt f (IxCoyo g x) = g >>> f $ x

mapCxt :: (a ~> b) -> IxCoyoneda a ~> IxCoyoneda b
mapCxt f (IxCoyo g x) = IxCoyo f (g x)

instance IxFunctor IxCoyoneda where
  imap f (IxCoyo g x) = IxCoyo (g >>> f) x

instance IxApplicative IxCoyoneda where
  ireturn = icoyo

instance IxMonad IxCoyoneda where
  ibind f (IxCoyo g x) = g >>> f $ x

programable :: a ~> IxFree IxCoyoneda a
programable = icoyo >>> imap IxPure >>> IxFree
