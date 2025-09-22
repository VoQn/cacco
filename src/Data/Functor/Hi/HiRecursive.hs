{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Hi.HiRecursive where

--
import Control.Arrow ((>>>))
import Data.Functor.Const (
    Const (..),
    getConst,
 )

--
import Data.Functor.Hi.HiFunctor
import Data.Functor.Hi.Types
import Data.Kind (Type)

-------------------------------------------------------------------------------
-- Higher-order Recursive
-------------------------------------------------------------------------------

-- | Higher-order version of the 'Recursive' type-class
class (HiFunctor (HiBase f)) => HiRecursive (f :: Type -> Type) where
    hproject :: f ~> (HiBase f) f

    -- | catamorphism folding for 'HiRecursive'
    hcata :: HiAlgebra (HiBase f) g -> f ~> g
    hcata phi = hproject >>> hmap (hcata phi) >>> phi

    -- | catamorphism folding returns constant value
    kcata :: KAlgebra (HiBase f) a -> f ~>. a
    kcata phi = hcata (phi >>> Const) >>> getConst

    hpara :: HiRAlgebra f g -> f ~> g
    hpara phi = hproject >>> hmap (hpara phi &&&& id) >>> phi

-- | Higher-order version of the 'CoRecursive' type-class
class (HiFunctor (HiBase f)) => HiCorecursive (f :: Type -> Type) where
    hembed :: (HiBase f) f ~> f

    -- | anamorphism unfolding
    hana :: HiCoalgebra g (HiBase f) -> g ~> f
    hana psi = psi >>> hmap (hana psi) >>> hembed

    -- | apomorphism unfolding
    hapo :: HiRCoalgebra g f -> g ~> f
    hapo psi = psi >>> hmap (id |||| hapo psi) >>> hembed
