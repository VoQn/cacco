{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Cacco.Syntax.Expr where

import           Data.Functor.Classes
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)

import           Data.Ann
import           Data.Fix

import           Cacco.Syntax.Literal (Literal)

-- | Abstruct syntax tree of Cacco language
data AstF r where
  -- Atomic
  -- | Hole @_@
  HolF :: AstF r
  -- | 'Literal'
  LitF :: Literal -> AstF r
  -- | Symbol
  SymF :: String -> AstF r

  -- Collections
  -- | List
  LisF :: [r] -> AstF r
  -- | Struct
  StrF :: [(String, r)] -> AstF r

  -- Fuctors
  -- | Apply function
  AppF :: r -> [r] -> AstF r
  -- | Lambda (anonymous) function
  LamF :: [r] -> r -> AstF r
  -- | Declare a constant value
  ConF :: r -> r -> AstF r
  deriving (Eq, Show, Typeable, Generic, Functor, Foldable, Traversable)

instance Eq1 AstF where
  liftEq eq astX astY = case (astX, astY) of
      (HolF, HolF)             -> True
      (LitF x, LitF y)         -> x == y
      (SymF x, SymF y)         -> x == y
      (LisF x, LisF y)         -> liftEq eq x y
      (StrF x, StrF y)         -> liftEq eqPair x y
      (AppF f1 a1, AppF f2 a2) -> f1 `eq` f2 && liftEq eq a1 a2
      (LamF p1 b1, LamF p2 b2) -> liftEq eq p1 p2 && b1 `eq` b2
      (ConF n1 e1, ConF n2 e2) -> n1 `eq` n2 && e1 `eq` e2
      (_, _)                   -> False
    where
      eqPair (n, x) (m, y) = n == m && x `eq` y

instance Show1 AstF where
  liftShowsPrec _ _ _ _ = showString ""

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

pattern Struct :: [(String, Ast)] -> Ast
pattern Struct map = Fix (StrF map)

pattern App :: Ast -> [Ast] -> Ast
pattern App fn args = Fix (AppF fn args)

pattern Lam :: [Ast] -> Ast -> Ast
pattern Lam params body = Fix (LamF params body)

pattern Con :: Ast -> Ast -> Ast
pattern Con name expr = Fix (ConF name expr)

-- | 'Expr' is some annotated Fixed 'AstF'
type Expr i = Ann i AstF
