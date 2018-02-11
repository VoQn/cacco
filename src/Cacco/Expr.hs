{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cacco.Expr where

import           Cacco.Fix
import           Cacco.Literal (Literal)
import qualified Cacco.Literal as Lit
import           Data.Map      (Map)
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

data ExprF a
  = LitF Lit.Literal
  -- | Symbol
  | SymF String
  -- | Linked list
  | LisF [a]
  -- | Fixed size vector
  | VecF [a]
  -- | Struct
  | StrF (Map String a)
  deriving (Eq, Ord, Show, Typeable, Generic, Functor)

type Expr = Fix ExprF

pattern Literal :: Literal -> Expr
pattern Literal lit = Fix (LitF lit)

pattern Symbol :: String -> Expr
pattern Symbol sym  = Fix (SymF sym)

pattern List :: [Expr] -> Expr
pattern List vals   = Fix (LisF vals)

pattern Vector :: [Expr] -> Expr
pattern Vector vals = Fix (VecF vals)

pattern Struct :: Map String Expr -> Expr
pattern Struct map  = Fix (StrF map)

data Info i f a = Info
  {
    info    :: i,
    content :: f a
  } deriving (Eq, Show, Typeable, Generic, Functor)

type AnnExpr i = Fix (Info i ExprF)

removeAnn :: AnnExpr i -> Expr
removeAnn (Fix (Info _ e)) = case e of
  LitF l  -> Literal l
  SymF n  -> Symbol n
  LisF vs -> List $ removeAnn <$> vs
  VecF vs -> Vector $ removeAnn <$> vs

isAtomic :: ExprF a -> Bool
isAtomic (LisF _) = False
isAtomic (VecF _) = False
isAtomic (StrF _) = False
isAtomic _        = True

isCollection :: ExprF a -> Bool
isCollection (LisF _) = True
isCollection (VecF _) = True
isCollection (StrF _) = True
isCollection _        = False
