{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Functor.Ix where

import           Control.Applicative
import           Data.Function         (on)
import           Data.Functor.Classes  ()
import           Data.Functor.Const    ()
import           Data.Functor.Identity ()
import           Data.Typeable         (Typeable)

-- | Natural transformation
type f ~> g = forall a. f a -> g a
infix 5 ~>

-- | Constant on the left
type f .~> g = forall a. f -> g a
infixr 5 .~>

-- | Constant on the right
type f ~>. g = forall a. f a -> g
infixr 5 ~>.

type a ~>> b = forall f. IxFunctor f => f a ~> a -> f b ~> b

type Lim (f :: k -> *) = forall (a :: k). f a

type family IxBase (h :: k -> *) :: (k -> *) -> (k -> *)

type IxAlgebra h f = h f ~> f
type KAlgebra h f a = h f ~>. a
type IxCoalgebra h f = f ~> h f

-------------------------------------------------------------------------------
-- Indexed Functor
-------------------------------------------------------------------------------

-- | Indexed functor
class IxFunctor (h :: (i -> *) -> (i -> *)) where
    -- | Indexed 'fmap'
    imap :: (f ~> g) -> (h f ~> h g)

    -- | hylomorphism
    ihylo :: IxAlgebra h g -> IxCoalgebra h f -> f ~> g
    ihylo phi psi = phi . imap (ihylo phi psi) . psi

-- | The product of indexed functors
data (f :*: g) a = (:*:) { ifst :: f a, isnd :: g a }
infixr 6 :*:

(&&&&) :: (f ~> g) -> (f ~> g') -> f ~> (g :*: g')
(&&&&) u v x = u x :*: v x
infixr 3 &&&&

iunzip :: IxFunctor h => h (f :*: g) ~> (h f :*: h g)
iunzip = imap ifst &&&& imap isnd

-------------------------------------------------------------------------------
-- Indexed Recursive
-------------------------------------------------------------------------------

class IxFunctor (IxBase h) => IxRecursive (h :: i -> *) where
    iproject :: h ~> (IxBase h) h

    -- | catamorphism folding
    icata :: IxAlgebra (IxBase h) f -> h ~> f
    icata g = g . imap (icata g) . iproject

    -- | catamorphism folding returning constant type
    icata' :: KAlgebra (IxBase h) (Const a) a -> h ~>. a
    icata' g = getConst . icata (Const . g)

    -- | paramorphism folding
    ipara :: ((IxBase h) (a :*: h) ~> a) -> h ~> a
    ipara f = f . imap (ipara f &&&& id) . iproject

class IxFunctor (IxBase h) => IxCorecursive (h :: i -> *) where
    iembed :: (IxBase h) h ~> h

    iana :: IxCoalgebra (IxBase h) f -> f ~> h
    iana g = iembed . imap (iana g) . g

class IxFoldable (h :: (i -> *) -> l -> *) where
    ifoldMap :: forall (f :: i -> *) (m :: *).
                Monoid m => f ~>. m -> h f ~>. m

class IxFunctor h => IxTraversable h where
    itraverse :: Applicative f
              => (forall i. a i -> f (b i))
              -> (forall j. h a j -> f (h b j))
-------------------------------------------------------------------------------
-- Indexed Fix
-------------------------------------------------------------------------------

-- | Indexed Fix
newtype IxFix f i = IxFix (f (IxFix f) i)

-- | unfix for 'IxFix' version
iunfix :: IxFix f i -> f (IxFix f) i
iunfix (IxFix f) = f

deriving instance Eq (f (IxFix f) i) => Eq (IxFix f i)
deriving instance Ord (f (IxFix f) i) => Ord (IxFix f i)
deriving instance Show (f (IxFix f) i) => Show (IxFix f i)
deriving instance Typeable (IxFix f)
deriving instance Functor (f (IxFix f)) => Functor (IxFix f)
deriving instance Foldable (f (IxFix f)) => Foldable (IxFix f)
deriving instance Traversable (f (IxFix f)) => Traversable (IxFix f)

type instance IxBase (IxFix f) = f

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

-------------------------------------------------------------------------------
-- Indexed Equality
-------------------------------------------------------------------------------

class IxEq (f :: k -> *) where
    ieq :: f i -> f i -> Bool

data (:=:) x y where
    Refl :: x :=: x

class IxEq f => IxEqHet f where
    -- | type index equality
    ieqIdx :: f a -> f b -> Maybe (a :=: b)
    -- | heterogeneous equality
    ieqHet :: f a -> f b -> Maybe (a :=: b)
    ieqHet x y = case ieqIdx x y of
        Just Refl | x `ieq` y -> Just Refl
        _________ -> Nothing

instance IxEq (f (IxFix f)) => IxEq (IxFix f) where
    ieq = ieq `on` iunfix

data Some f = forall a. Some (f a)

some :: (forall a. f a -> b) -> Some f -> b
some f (Some x) = f x

-------------------------------------------------------------------------------
-- Indexed Free
-------------------------------------------------------------------------------

data IxFreeF f a b i where
    IxPureF :: a i -> IxFreeF f a b i
    IxFreeF :: f b i -> IxFreeF f a b i

data IxFree f a i where
    IxPure :: a i -> IxFree f a i
    IxFree :: f (IxFree f a) i -> IxFree f a i

type instance IxBase (IxFree f a) = (IxFreeF f a)

instance IxFunctor f => IxFunctor (IxFree f) where
    imap f (IxPure a) = IxPure $ f a
    imap f (IxFree w) = IxFree $ imap f `imap` w
