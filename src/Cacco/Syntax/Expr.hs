{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cacco.Syntax.Expr where

import           Data.Map.Lazy        (Map)
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)

import           Cacco.Ann
import           Cacco.Fix
import           Cacco.Syntax.Literal (Literal)

-- | Abstruct syntax tree of Cacco language
data AstF a
  -- Atomic
  -- | Hole @_@
  = HolF
  -- | 'Literal'
  | LitF Literal
  -- | Symbol
  | SymF String

  -- Collections
  -- | Linked list
  | LisF [a]
  -- | Fixed size vector
  | VecF [a]
  -- | Struct
  | StrF (Map String a)

  -- Fuctors
  -- | Apply function
  | AppF  a [a]
  -- | Lambda (anonymous) function
  | LamF [a] a
  -- | Declare a constant value
  | ConF String a
  deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable)

instance Traversable AstF where
  traverse _ HolF        = pure HolF
  traverse _ (SymF x)    = SymF <$> pure x
  traverse _ (LitF x)    = LitF <$> pure x
  traverse f (LisF xs)   = LisF <$> traverse f xs
  traverse f (VecF xs)   = VecF <$> traverse f xs
  traverse f (StrF xs)   = StrF <$> traverse f xs
  traverse f (AppF x xs) = AppF <$> f x <*> traverse f xs
  traverse f (LamF xs x) = LamF <$> traverse f xs <*> f x
  traverse f (ConF n  x) = ConF <$> pure n <*> f x

-- | 'Fix'ed version 'AstF'
type Ast = Fix AstF

pattern Hole :: Ast
pattern Hole = Fix HolF

pattern Literal :: Literal -> Ast
pattern Literal lit = Fix (LitF lit)

pattern Symbol :: String -> Ast
pattern Symbol sym = Fix (SymF sym)

pattern List :: [Ast] -> Ast
pattern List vals = Fix (LisF vals)

pattern Vector :: [Ast] -> Ast
pattern Vector vals = Fix (VecF vals)

pattern Struct :: Map String Ast -> Ast
pattern Struct map = Fix (StrF map)

pattern App :: Ast -> [Ast] -> Ast
pattern App fn args = Fix (AppF fn args)

pattern Lam :: [Ast] -> Ast -> Ast
pattern Lam params body = Fix (LamF params body)

pattern Con :: String -> Ast -> Ast
pattern Con name expr = Fix (ConF name expr)

-- | 'Expr' is some annotated Fixed 'AstF'
type Expr i = Ann i AstF
