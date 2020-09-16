{-# LANGUAGE GADTs          #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Data.Functor.Ix.IxEq where

-------------------------------------------------------------------------------
-- Indexed Equality
-------------------------------------------------------------------------------

class IxEq (f :: k -> *) where
    ieq :: f i -> f i -> Bool

data x :=: y where
    Refl ::x :=: x

class IxEq f => IxEqHet f where
    -- | type index equality
    ieqIdx :: f a -> f b -> Maybe (a :=: b)
    -- | heterogeneous equality
    ieqHet :: f a -> f b -> Maybe (a :=: b)
    ieqHet x y = case ieqIdx x y of
        Just Refl | ieq x y -> Just Refl
        _________ -> Nothing
