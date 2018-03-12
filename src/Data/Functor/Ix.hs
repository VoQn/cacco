{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PatternSynonyms           #-}
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
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

infixr 6 :*:

type (x :& g) = Const x :*: g
pattern (:&) :: a -> f i -> (Const a :*: f) i
pattern x :& g = Const x :*: g

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

class IxFoldable t where
    ifoldMap :: Monoid m => f ~>. m -> t f ~>. m

class IxFunctor t => IxTraversable t where
    itraverse :: Applicative f
              => (forall i. a i -> f (b i))
              -> (forall j. t a j -> f (t b j))

-------------------------------------------------------------------------------
-- Indexed Fix
-------------------------------------------------------------------------------

-- | Indexed Fix
newtype IxFix f i = IxFix (f (IxFix f) i)

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
        Just Refl | ieq x y -> Just Refl
        _________ -> Nothing

instance IxEq (f (IxFix f)) => IxEq (IxFix f) where
    ieq = ieq `on` iunfix

data Some f = forall a. Some (f a)

some :: f ~>. b -> Some f -> b
some f (Some x) = f x

-------------------------------------------------------------------------------
-- Indexed Free
-------------------------------------------------------------------------------
-- | Indexed Free
data IxFree h f i where
    IxPure   :: f i -> IxFree h f i
    IxImpure :: h (IxFree h f) i -> IxFree h f i

instance (IxEq f, IxEq (h (IxFree h f))) => IxEq (IxFree h f) where
    ieq (IxPure a) (IxPure b)     = ieq a b
    ieq (IxImpure f) (IxImpure g) = ieq f g
    ieq _ _                       = False

deriving instance (Eq (f i), Eq (h (IxFree h f) i)) => Eq (IxFree h f i)
deriving instance (Show (f i), Show (h (IxFree h f) i)) => Show (IxFree h f i)
deriving instance (Functor f, Functor (h (IxFree h f))) => Functor (IxFree h f)

-- | Base Functor for Indexed Free
data IxFreeF h f g i where
    IxPureF   :: f i -> IxFreeF h f g i
    IxImpureF :: h g i -> IxFreeF h f g i

type instance IxBase (IxFree f a) = (IxFreeF f a)

instance IxFunctor f => IxFunctor (IxFreeF f a) where
    imap _ (IxPureF   a) = IxPureF a
    imap f (IxImpureF h) = IxImpureF $ imap f h

instance IxFunctor f => IxRecursive (IxFree f a) where
    iproject (IxPure   f) = IxPureF f
    iproject (IxImpure f) = IxImpureF f

instance IxFunctor f => IxCorecursive (IxFree f a) where
    iembed (IxPureF   f) = IxPure f
    iembed (IxImpureF f) = IxImpure f

instance IxFunctor f => IxFunctor (IxFree f) where
    imap f (IxPure   a) = IxPure $ f a
    imap f (IxImpure w) = IxImpure $ imap (imap f) w

instance IxFoldable f => IxFoldable (IxFree f) where
    ifoldMap f (IxPure   a) = f a
    ifoldMap f (IxImpure w) = ifoldMap (ifoldMap f) w

instance IxTraversable f => IxTraversable (IxFree f) where
    itraverse f (IxPure   a) = IxPure <$> f a
    itraverse f (IxImpure w) = IxImpure <$> itraverse (itraverse f) w

ifree :: IxFreeF f a (IxFree f a) ~> IxFree f a
ifree (IxPureF   a) = IxPure a
ifree (IxImpureF f) = IxImpure f

-------------------------------------------------------------------------------
-- Indexed Cofree
-------------------------------------------------------------------------------
-- | Indexed Cofree
data IxCofree h f i = (f i) :< (h (IxCofree h f) i)

-- | Base Functor for Indexed Cofree
data IxCofreeF h f g i = (f i) :<< (h g i)

type instance IxBase (IxCofree h f) = IxCofreeF h f

instance IxFunctor f => IxFunctor (IxCofreeF f a) where
    imap f (x :<< xs) = x :<< imap f xs

instance IxFunctor f => IxRecursive (IxCofree f a) where
    iproject (x :< xs) = x :<< xs

instance IxFunctor f => IxCorecursive (IxCofree f a) where
    iembed (x :<< xs) = x :< xs

icofree :: IxCofreeF h f (IxCofree h f) ~> IxCofree h f
icofree (x :<< xs) = x :< xs
