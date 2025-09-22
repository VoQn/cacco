{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Ix.IxRecursive where

import Data.Kind (Type)

import Data.Functor.Const (
    Const (Const),
    getConst,
 )

import Data.Functor.Ix.IxFunctor
import Data.Functor.Ix.Types

-------------------------------------------------------------------------------
-- Indexed Recursive & Corecursive
-------------------------------------------------------------------------------

class (IxFunctor (IxBase f)) => IxRecursive (f :: i -> Type) where
    iproject :: f ~> (IxBase f) f

    -- | catamorphism folding
    icata :: IxAlgebra (IxBase f) a -> f ~> a
    icata g = g . imap (icata g) . iproject

    -- | catamorphism folding returning constant type
    icata' :: KAlgebra (IxBase f) a -> f ~>. a
    icata' g = getConst . icata (Const . g)

    -- | paramorphism folding
    ipara :: ((IxBase f) (a :*: f) ~> a) -> f ~> a
    ipara f = f . imap (ipara f &&&& id) . iproject

class (IxFunctor (IxBase f)) => IxCorecursive (f :: i -> Type) where
    iembed :: (IxBase f) f ~> f

    iana :: IxCoalgebra (IxBase f) a -> a ~> f
    iana g = iembed . imap (iana g) . g
