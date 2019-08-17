{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Functor.Ix.IxFunctor where
--
import           Control.Arrow
import           Data.Functor.Ix.Types

-------------------------------------------------------------------------------
-- Indexed Functor
-------------------------------------------------------------------------------

-- | Indexed functor
class IxFunctor (f :: (i -> *) -> (i -> *)) where
    -- | Indexed 'fmap'
    imap :: (a ~> b) -> (f a ~> f b)

    -- | hylomorphism
    ihylo :: IxAlgebra f b -> IxCoalgebra f a -> a ~> b
    ihylo f g = g >>> imap (ihylo f g) >>> f

-- | 'unzip'
iunzip :: IxFunctor f => f (a :*: b) ~> (f a :*: f b)
iunzip = imap fst1 &&&& imap snd1

-- | Indexed Applicative
class IxFunctor f => IxApplicative f where
    ireturn :: IxCoalgebra f a

class IxApplicative m => IxMonad m where
    ibind :: (a ~> m b) -> m a ~> m b
    ibind f = imap f >>> ijoin

    ijoin :: m (m a) ~> m a
    ijoin = ibind id

(>~>) :: IxMonad m => (a ~> m b) -> (b ~> m c) -> a ~> m c
f >~> g = f >>> ibind g

(<~<) :: IxMonad m => (b ~> m c) -> (a ~> m b) -> a ~> m c
f <~< g = g >>> ibind f

iliftM :: IxMonad m => (a ~> b) -> m a ~> m b
iliftM f = ibind (f >>> ireturn)

(?>=) :: IxMonad m => m a i -> (a ~> m b) -> m b i
m ?>= f = ibind f m
