{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Cacco.Fix where

newtype Fix f = Fix { out :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

fold :: Functor f
  => (f a -> a)
  -> Fix f
  -> a
fold acc (Fix f) = acc (fold acc <$> f)
