{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}
module Data.Functor.Ix.IxRecursive where

import           Data.Functor.Const

import           Data.Functor.Ix.IxFunctor
import           Data.Functor.Ix.Types

-------------------------------------------------------------------------------
-- Indexed Recursive
-------------------------------------------------------------------------------

class IxFunctor (IxBase h) => IxRecursive (h :: i -> *) where
    iproject :: h ~> (IxBase h) h

    -- | catamorphism folding
    icata :: IxAlgebra (IxBase h) f -> h ~> f
    icata g = g . imap (icata g) . iproject

    -- | catamorphism folding returning constant type
    icata' :: KAlgebra (IxBase h) a -> h ~>. a
    icata' g = getConst . icata (Const . g)

    -- | paramorphism folding
    ipara :: ((IxBase h) (a :*: h) ~> a) -> h ~> a
    ipara f = f . imap (ipara f &&&& id) . iproject

class IxFunctor (IxBase h) => IxCorecursive (h :: i -> *) where
    iembed :: (IxBase h) h ~> h

    iana :: IxCoalgebra (IxBase h) f -> f ~> h
    iana g = iembed . imap (iana g) . g
