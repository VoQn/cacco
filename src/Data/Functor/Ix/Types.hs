{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Functor.Ix.Types where
--
import           Data.Typeable
--
import           Data.Functor.Const (Const (..))

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

type Lim (f :: k -> *) = forall (a :: k). f a

type family Indexed (ix :: k) :: k -> *

type family IxBase (h :: k -> *) :: (k -> *) -> k -> *

-- | Indexed algebric function
type IxAlgebra h f = h f ~> f

-- | algebric function returns constant
type KAlgebra h k = h (Const k) ~>. k

-- | Indexed coalgebric function
type IxCoalgebra h f = f ~> h f

-- | Monadic natural transformation
type NatM m f g = forall a. f a -> m (g a)

--
data Some f = forall a. Some (f a)

some :: f ~>. b -> Some f -> b
some f (Some x) = f x
-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------
infixr 6 :*:

-- | The product of indexed functors
data (f :*: g) a = f a :*: g a
    deriving (Eq, Ord, Show, Read, Typeable, Functor, Foldable, Traversable)

type (x :& g) = Const x :*: g
pattern (:&) :: a -> f i -> (Const a :*: f) i
pattern x :& g = Const x :*: g

fst1 :: (f :*: g) ~> f
fst1 (f :*: _) = f

snd1 :: (f :*: g) ~> g
snd1 (_ :*: g) = g

infixr 3 &&&&
(&&&&) :: f ~> g -> f ~> h -> f ~> (g :*: h)
(&&&&) f g x = f x :*: g x

data (f :+: g) a = L1 (f a) | R1 (g a)
    deriving (Eq, Ord, Show, Read, Typeable, Functor, Foldable, Traversable)

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

-- | Indexed R-Algebra
type IxRAlgebra f g = (IxBase f) (g :*: f) ~> g

-- | Indexed R-Coalgebra
type IxRCoalgebra f g = g ~> (IxBase f) (f :+: g)
