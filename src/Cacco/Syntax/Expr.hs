{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cacco.Syntax.Expr where

import           Cacco.Syntax.Literal           ( Literal )
import           Data.Ann                       ( Ann )
import           Data.Functor.Foldable.TH       ( makeBaseFunctor )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )

-- | Abstruct syntax tree of Cacco language
data Ast
  {- Atomics -}
  -- | Hole @_@
  = Hole
  -- | 'Literal'
  | Lit Literal
  -- | Symbol
  | Sym String

  {- Collections -}
  -- | List
  | List [Ast]
  -- | Struct
  | StrF [(String, Ast)]

  {- Fuctors -}
  -- | Apply function
  | App Ast [Ast]
  -- | Lambda (anonymous) function
  | Lam [Ast] Ast
  -- | Declare a constant value
  | Con Ast Ast
  deriving (Eq, Show, Typeable, Generic)

makeBaseFunctor ''Ast

type Expr i = Ann i AstF
