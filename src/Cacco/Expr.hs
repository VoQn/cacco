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

module Cacco.Expr where

import           Data.Map.Lazy (Map)
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import           Cacco.Ann
import           Cacco.Fix
import           Cacco.Literal (Literal)

-- | Abstruct syntax tree of Cacco language
data AstF a
  -- Atomic
  = HolF         -- ^ Hole @_@
  | LitF Literal -- ^ 'Literal'
  | SymF String  -- ^ Symbol

  -- Collections
  | LisF [a] -- ^ Linked list
  | VecF [a] -- ^ Fixed size vector
  | StrF (Map String a) -- ^ Struct

  -- Fuctors
  | AppF  a [a] -- ^ Apply function
  | LamF [a] a  -- ^ Lambda (anonymous) function
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

-- | 'Expr' is some annotated Fixed 'AstF'
type Expr i = Ann i AstF
