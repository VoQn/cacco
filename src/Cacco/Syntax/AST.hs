{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cacco.Syntax.AST (
    Ast (..),
    AstF (..),
)
where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.List.NonEmpty (
    NonEmpty (..),
    toList,
 )

import qualified Data.Kind as K (Type)
import Data.Typeable (Typeable)
import Prettyprinter

import Cacco.Syntax.Index
import Cacco.Syntax.Literal
import Data.Functor.Ix

-------------------------------------------------------------------------------
-- Indexed AST
-------------------------------------------------------------------------------

data Ast (i :: Index) where
    -- Atomic (Leaf Node of AST)

    -- | Hole @_@
    Hole :: IndexProxy i -> Ast i
    -- | 3 Dots @...@
    Dots :: IndexProxy i -> Ast i
    -- | 'Literal'
    Lit :: IndexProxy i -> Literal -> Ast i
    -- | Variable
    Var :: IndexProxy i -> String -> Ast i
    -- Collections

    -- | List, Array, Vector Expression
    Vec :: [Ast i] -> Ast i
    -- | Key-Value Map
    Map :: [(Ast i, Ast i)] -> Ast i
    -- Conditional branch

    -- | if
    If :: Ast 'Expr -> Ast 'Expr -> Ast 'Expr -> Ast 'Expr
    -- | multyway if
    Ifs :: [(Ast 'Expr, Ast 'Expr)] -> Ast 'Expr -> Ast 'Expr
    -- | Pattern-match
    Case :: [(Ast 'Patt, Ast 'Expr)] -> Ast 'Expr -> Ast 'Expr
    -- | Apply function
    App :: Ast i -> [Ast i] -> Ast i
    -- | Lambda (anonymous) function
    Lam :: [Ast 'Patt] -> NonEmpty (Ast 'Expr) -> Ast 'Expr
    -- Declarations

    -- | Declare type bind
    Dec :: Ast 'Patt -> NonEmpty (Ast 'Type) -> Ast 'Expr
    -- | Define function or constants
    Def :: Ast 'Patt -> NonEmpty (Ast 'Expr) -> Ast 'Expr

deriving instance Eq (Ast i)
deriving instance Show (Ast i)
deriving instance Typeable (Ast i)

-------------------------------------------------------------------------------
-- Indexed AST Base-Functor
-------------------------------------------------------------------------------

-- | Indexed Base Functor for Abstruct Syntax Tree of Cacco language
data AstF (r :: Index -> K.Type) (i :: Index) where
    -- Atomic (Leaf Node of AST)

    -- | Hole @_@
    HoleF :: IndexProxy i -> AstF r i
    -- | 3 Dots @...@
    DotsF :: IndexProxy i -> AstF r i
    -- | 'Literal'
    LitF :: IndexProxy i -> Literal -> AstF r i
    -- | Variable
    VarF :: IndexProxy i -> String -> AstF r i
    -- Collections

    -- | List, Array, Vector Expression
    VecF :: [r i] -> AstF r i
    -- | Key-Value Map
    MapF :: [(r i, r i)] -> AstF r i
    -- Conditional branch

    -- | if
    IfF :: r 'Expr -> r 'Expr -> r 'Expr -> AstF r 'Expr
    -- | multyway if
    IfsF :: [(r 'Expr, r 'Expr)] -> r 'Expr -> AstF r 'Expr
    -- | Pattern-match
    CaseF :: [(r 'Patt, r 'Expr)] -> r 'Expr -> AstF r 'Expr
    -- | Apply function
    AppF :: r i -> [r i] -> AstF r i
    -- | Lambda (anonymous) function
    LamF :: [r 'Patt] -> NonEmpty (r 'Expr) -> AstF r 'Expr
    -- Declarations

    -- | Declare type bind
    DecF :: r 'Patt -> NonEmpty (r 'Type) -> AstF r 'Expr
    -- | Define function or constants
    DefF :: r 'Patt -> NonEmpty (r 'Expr) -> AstF r 'Expr

deriving instance
    forall (r :: Index -> K.Type) (i :: Index).
    ( Show (r 'Expr)
    , Show (r 'Patt)
    , Show (r 'Decl)
    , Show (r 'Type)
    , Show (r i)
    ) =>
    Show (AstF r i)

deriving instance
    forall (r :: Index -> K.Type) (i :: Index).
    ( Eq (r 'Expr)
    , Eq (r 'Patt)
    , Eq (r 'Decl)
    , Eq (r 'Type)
    , Eq (r i)
    ) =>
    Eq (AstF r i)

deriving instance
    forall (r :: Index -> K.Type) (i :: Index).
    ( Typeable (r 'Expr)
    , Typeable (r 'Patt)
    , Typeable (r 'Decl)
    , Typeable (r 'Type)
    , Typeable (r i)
    ) =>
    Typeable (AstF r i)

type instance IxBase Ast = AstF

instance IxFunctor AstF where
    imap f astF = case astF of
        HoleF p -> HoleF p
        DotsF p -> DotsF p
        LitF p v -> LitF p v
        VarF p v -> VarF p v
        VecF v -> VecF (f <$> v)
        MapF v -> MapF (pairMap f f <$> v)
        IfF c t e -> IfF (f c) (f t) (f e)
        IfsF c o -> IfsF (pairMap f f <$> c) (f o)
        CaseF c o -> CaseF (pairMap f f <$> c) (f o)
        AppF g a -> AppF (f g) (f <$> a)
        LamF a e -> LamF (f <$> a) (f <$> e)
        DecF v t -> DecF (f v) (f <$> t)
        DefF v e -> DefF (f v) (f <$> e)
      where
        pairMap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
        pairMap f' g' = f' . fst &&& g' . snd

instance IxRecursive Ast where
    iproject ast = case ast of
        Hole p -> HoleF p
        Dots p -> DotsF p
        Lit p v -> LitF p v
        Var p v -> VarF p v
        Vec v -> VecF v
        Map v -> MapF v
        If c t e -> IfF c t e
        Ifs p o -> IfsF p o
        Case p o -> CaseF p o
        App f a -> AppF f a
        Lam a e -> LamF a e
        Dec v t -> DecF v t
        Def v e -> DefF v e

instance IxCorecursive Ast where
    iembed astF = case astF of
        HoleF p -> Hole p
        DotsF p -> Dots p
        LitF p c -> Lit p c
        VarF p v -> Var p v
        VecF v -> Vec v
        MapF v -> Map v
        IfF c t e -> If c t e
        IfsF p o -> Ifs p o
        CaseF p o -> Case p o
        AppF f a -> App f a
        LamF a e -> Lam a e
        DecF v t -> Dec v t
        DefF v e -> Def v e

-------------------------------------------------------------------------------
-- Prettify
-------------------------------------------------------------------------------

instance Pretty (Ast i) where
    pretty = icata' alg
      where
        alg :: forall ann. AstF (Const (Doc ann)) ~>. Doc ann
        alg astF = case astF of
            HoleF _ -> "_"
            DotsF _ -> "..."
            LitF _ l -> pretty l
            VarF _ v -> pretty v
            VecF v -> brackets . sep $ getConst <$> v
            MapF v -> braces . sep $ _prettyKeyValue <$> v
            IfF (Const c) (Const t) (Const e) -> "(if" <+> c <+> t <+> e <> ")"
            IfsF p (Const o) -> "(if" <+> _prettyCases p o <> ")"
            CaseF p (Const o) -> "(case" <+> _prettyCases p o <> ")"
            AppF (Const f) args -> "(" <> f <+> sep (getConst <$> args) <> ")"
            LamF args es -> "(" <> pipeArgs args <+> asList es <> ")"
            DecF (Const v) t -> "(:" <+> v <+> asList t <> ")"
            DefF (Const v) e -> "(=" <+> v <+> asList e <> ")"
        {-# INLINE alg #-}

        pipeArgs :: forall ann (j :: Index). () => [Const (Doc ann) j] -> Doc ann
        pipeArgs args = "|" <> sep (getConst <$> args) <> "|"
        {-# INLINE pipeArgs #-}

        asList ::
            forall ann (j :: Index). () => NonEmpty (Const (Doc ann) j) -> Doc ann
        asList = sep . toList . fmap getConst
        {-# INLINE asList #-}

-- | pretty print Key-Value pair
_prettyKeyValue ::
    forall (j :: Index) (k :: Index) a.
    () =>
    (Const (Doc a) j, Const (Doc a) k) ->
    Doc a
_prettyKeyValue (Const x, Const y) = x <> ":" <+> y
{-# INLINE _prettyKeyValue #-}

-- | pretty print conditional case pair
_prettyCases ::
    forall (j :: Index) (k :: Index) ann.
    () =>
    [(Const (Doc ann) j, Const (Doc ann) k)] ->
    Doc ann ->
    Doc ann
_prettyCases patterns otherwiseCase = sep $ cap <$> allCases
  where
    allCases :: [(Doc ann, Doc ann)]
    allCases = (pair <$> patterns) ++ [("otherwise", otherwiseCase)]
    {-# INLINE allCases #-}

    pair :: (Const (Doc ann) j, Const (Doc ann) k) -> (Doc ann, Doc ann)
    pair (Const x, Const y) = (x, y)
    {-# INLINE pair #-}

    -- \| capping by brackets
    cap :: (Doc ann, Doc ann) -> Doc ann
    cap (p, e) = brackets $ p <+> e
    {-# INLINE cap #-}
{-# INLINE _prettyCases #-}
