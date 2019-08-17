{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Hi.Types where

import           Data.Functor.Const (Const)
import           GHC.Generics       ((:*:) (..), (:+:) (..))
-------------------------------------------------------------------------------
-- Type synonyms, Type operators, and Type families
-------------------------------------------------------------------------------

-- | Natural transformation
type f ~> g = forall a. f a -> g a
infixr 0 ~>

-- | Constant transformer on the left
type a .~> f = forall b. a -> f b
infixr 0 .~>

-- | Constant tranformer on the left
type f ~>. b = forall a. f a -> b
infixr 0 ~>.

-- | type family of Higher-order-functor's base functor
type family HiBase (h :: * -> *) :: (* -> *) -> (* -> *)

-- | Higher-order algebric function
type HiAlgebra h f = h f ~> f

-- | algebric function returns constant
type KAlgebra h k = h (Const k) ~>. k

-- | Higher-order coalgebric function
type HiCoalgebra f h = f ~> h f

-- | Monadic natural transformation
type NatM m f g = forall a. f a -> m (g a)

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

fst1 :: (f :*: g) ~> f
fst1 (f :*: _) = f

snd1 :: (f :*: g) ~> g
snd1 (_ :*: g) = g

infixr 3 &&&&
(&&&&) :: f ~> g -> f ~> h -> f ~> (g :*: h)
(&&&&) f g x = f x :*: g x

fromL1 :: (f :+: g) a -> Maybe (f a)
fromL1 (L1 f) = Just f
fromL1 ______ = Nothing

fromR1 :: (f :+: g) a -> Maybe (g a)
fromR1 (R1 g) = Just g
fromR1 ______ = Nothing

infixr 3 ||||
(||||) :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
(||||) l r x = case x of
    L1 f -> l f
    R1 g -> r g

-- | Higher-order R-Algebra
type HiRAlgebra f g = (HiBase f) (g :*: f) ~> g

-- | Higher-order R-Coalgebra
type HiRCoalgebra g f = g ~> (HiBase f) (f :+: g)
