{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Hi.HiFunctor where

import Control.Arrow ((>>>))
import GHC.Generics ((:*:))

import Data.Functor.Hi.Types (
    fst1,
    snd1,
    (&&&&),
    type (~>),
 )

import Data.Kind (Type)

-------------------------------------------------------------------------------
-- Higher-order Functor
-------------------------------------------------------------------------------

-- | Higher-order Functor
class HiFunctor (h :: (Type -> Type) -> (Type -> Type)) where
    -- | Higher-order functor's 'fmap'
    hmap :: (f ~> g) -> (h f ~> h g)

-- | hylomorphism refolding
hhylo ::
    forall (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) (g :: Type -> Type).
    () =>
    --
    (HiFunctor h) =>
    (h g ~> g) ->
    (f ~> h f) ->
    f ~> g
hhylo f g = g >>> hmap (hhylo f g) >>> f

-- | 'unzip'
hunzip :: (HiFunctor h) => h (f :*: g) ~> (h f :*: h g)
hunzip = hmap fst1 &&&& hmap snd1

-------------------------------------------------------------------------------
-- Higher-order Applicative
-------------------------------------------------------------------------------

-- | Higher-order Applicative
class (HiFunctor h) => HiApplicative h where
    -- | Higher-order 'return'
    hreturn :: f ~> h f

-------------------------------------------------------------------------------
-- Higher-order Monad
-------------------------------------------------------------------------------

-- | Higher-order Monad
class (HiApplicative m) => HiMonad m where
    -- | Higher-order 'bind'
    hbind :: (a ~> m b) -> m a ~> m b
    hbind f = hjoin . hmap f

    -- | Higher-order 'join'
    hjoin :: m (m a) ~> m a
    hjoin = hbind id

-- | Higher-order '>~>'
(>~>) :: (HiMonad m) => (a ~> m b) -> (b ~> m c) -> a ~> m c
f >~> g = hbind g . f

-- | Higher-order '<~<'
(<~<) :: (HiMonad m) => (b ~> m c) -> (a ~> m b) -> a ~> m c
f <~< g = hbind f . g

-- | Higher-order 'liftM'
hliftM :: (HiMonad m) => (a ~> b) -> m a ~> m b
hliftM f = hbind (hreturn . f)

-- | Higher-order '?>='
(?>=) :: (HiMonad m) => m f a -> (f ~> m g) -> m g a
m ?>= f = hbind f m

-------------------------------------------------------------------------------
-- Higher-order Comonad
-------------------------------------------------------------------------------

-- | Higher-order Comonad
class (HiFunctor w) => HiComonad w where
    -- | Higher-order 'extract'
    hextract :: w a ~> a

    -- | Higher-order 'extend'
    hextend :: (w a ~> b) -> w a ~> w b
    hextend f = hmap f . hduplicate

    -- | Higher-order 'duplicate'
    hduplicate :: w a ~> w (w a)
    hduplicate = hextend id

-- | Higher-order '>~>'
(~>~) :: (HiComonad w) => (w a ~> b) -> (w b ~> c) -> w a ~> c
f ~>~ g = hextend f >>> g

-- | Higher-order '<~<'
(~<~) :: (HiComonad w) => (w b ~> c) -> (w a ~> b) -> w a ~> c
f ~<~ g = hextend g >>> f

-- | Higher-order '?=>'
(?=>) :: (HiComonad w) => w f a -> (w f ~> g) -> w g a
w ?=> f = hextend f w

-- | Higher-order 'liftW'
hliftW :: (HiComonad w) => (f ~> g) -> w f ~> w g
hliftW f = hextend (hextract >>> f)
