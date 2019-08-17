{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cacco.Syntax.Index where

import           Control.DeepSeq                ( NFData )
import           Data.Data                      ( Data )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )

data Index
    = Expr
    | Decl
    | Patt
    | Type
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Index

type Expr = 'Expr
type Decl = 'Decl
type Patt = 'Patt
type Type = 'Type

data IndexProxy (i :: Index) where
    ExprProxy ::IndexProxy 'Expr
    DeclProxy ::IndexProxy 'Decl
    PattProxy ::IndexProxy 'Patt
    TypeProxy ::IndexProxy 'Type

deriving instance Show (IndexProxy i)
deriving instance Eq (IndexProxy i)
deriving instance Ord (IndexProxy i)
deriving instance Typeable i => Typeable (IndexProxy i)
