{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Functor.Hi.HiFunctor where

import           Control.Arrow                  ( (>>>) )
import           GHC.Generics                   ( (:*:) )

import           Data.Functor.Hi.Types          ( type (~>)
                                                , fst1
                                                , snd1
                                                , (&&&&)
                                                )

-------------------------------------------------------------------------------
-- Higher-order Functor
-------------------------------------------------------------------------------

-- | Higher-order Functor
class HiFunctor (h :: (* -> *) -> (* -> *)) where
    -- | Higher-order functor's fmap'
    hmap :: (f ~> g) -> (h f ~> h g)

-- | hylomorphism refolding
hhylo
  :: forall (h :: (* -> *) -> * -> *) (f :: * -> *) (g :: * -> *)
   . ()
  => HiFunctor h => (h g ~> g) -> (f ~> h f) -> f ~> g
hhylo f g = g >>> hmap (hhylo f g) >>> f

hunzip :: HiFunctor h => h (f :*: g) ~> (h f :*: h g)
hunzip = hmap fst1 &&&& hmap snd1

-------------------------------------------------------------------------------
-- Higher-order Applicative
-------------------------------------------------------------------------------

-- | Higher-order Applicative
class HiFunctor h => HiApplicative h where
    hreturn :: f ~> h f

-------------------------------------------------------------------------------
-- Higher-order Monad
-------------------------------------------------------------------------------

-- | Higher-order Monad
class HiApplicative m => HiMonad m where
    hbind :: (a ~> m b) -> m a ~> m b
    hbind f = hjoin . hmap f

    hjoin :: m (m a) ~> m a
    hjoin = hbind id

(>~>) :: HiMonad m => (a ~> m b) -> (b ~> m c) -> a ~> m c
f >~> g = hbind g . f

(<~<) :: HiMonad m => (b ~> m c) -> (a ~> m b) -> a ~> m c
f <~< g = hbind f . g

hliftM :: HiMonad m => (a ~> b) -> m a ~> m b
hliftM f = hbind (hreturn . f)

(?>=) :: HiMonad m => m f a -> (f ~> m g) -> m g a
m ?>= f = hbind f m

-------------------------------------------------------------------------------
-- Higher-order Comonad
-------------------------------------------------------------------------------

class HiFunctor w => HiComonad w where
    hextract :: w a ~> a

    hextend :: (w a ~> b) -> w a ~> w b
    hextend f = hmap f . hduplicate

    hduplicate :: w a ~> w (w a)
    hduplicate = hextend id

(~>~) :: HiComonad w => (w a ~> b) -> (w b ~> c) -> w a ~> c
f ~>~ g = hextend f >>> g

(~<~) :: HiComonad w => (w b ~> c) -> (w a ~> b) -> w a ~> c
f ~<~ g = hextend g >>> f

(?=>) :: HiComonad w => w f a -> (w f ~> g) -> w g a
w ?=> f = hextend f w

hliftW :: HiComonad w => (f ~> g) -> w f ~> w g
hliftW f = hextend (hextract >>> f)
