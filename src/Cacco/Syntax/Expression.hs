{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cacco.Syntax.Expression where

import           Control.Arrow             ((&&&), (>>>))
import           Data.Functor.Const
import           Data.Proxy
import           Data.Text.Prettyprint.Doc
import           Data.Typeable             (Typeable)

import           Cacco.Syntax.ContextIndex (ContextIndex, DeclarationContext,
                                            ExpressionContext, PatternContext,
                                            TypingContext)
import           Cacco.Syntax.Term         (Term (..))

-- | Natural transformation
type f ~> g = forall i. f i -> g i
infixr 5 ~>

-- | Constant on the left
type f .~> g = forall i. f -> g i
infixr 5 .~>

-- | Constant on the right
type f ~>. g = forall i. f i -> g
infixr 5 ~>.

type family IBase (f :: i -> *) :: (i -> *) -> (i -> *)

type IAlgebra h f = h f ~> f
type ICoalgebra h f = f ~> h f

class IFunctor (h :: (i -> *) -> (i -> *)) where
    ifmap :: (f ~> g) -> (h f ~> h g)

    (<<$>>) :: (f ~> g) -> (h f ~> h g)
    (<<$>>) = ifmap

    (<<$) :: forall (f :: i -> *) (g :: i -> *). (forall (x :: i). f x) -> h g ~> h f
    l <<$ r = const l <<$>> r

class IFunctor (IBase h) => IRecursive (h :: i -> *) where
    iproject :: h ~> (IBase h) h

    icata :: IAlgebra (IBase h) f -> h ~> f
    icata phi = iproject >>> ifmap (icata phi) >>> phi

    icata' :: ((IBase h) (Const a) ~>. a) -> h ~>. a
    icata' phi = icata (Const . phi) >>> getConst

class IFunctor (IBase h) => ICorecursive (h :: i -> *) where
    iembed :: (IBase h) h ~> h

    iana :: ICoalgebra (IBase h) f -> f ~> h
    iana psi = psi >>> ifmap (iana psi) >>> iembed

ihylo :: IFunctor f => IAlgebra f b -> ICoalgebra f a -> a ~> b
ihylo f g = g >>> ifmap (ihylo f g) >>> f

data IFree h f i where
    IPure :: f i -> IFree h f i
    IFree :: h (IFree h f) i -> IFree h f i

instance IFunctor f => IFunctor (IFree f) where
    ifmap f (IPure a) = IPure (f a)
    ifmap f (IFree w) = IFree $ (f <<$>>) <<$>> w

-- | Expression
data Expression (i :: ContextIndex) where
    -- | Term
    Term :: Term -> Proxy i -> Expression i

    -- | List @[x y z]@
    List :: [Expression i] -> Expression i

    -- | KeyValue map @{:key value}@
    KeyValue :: [(String, Expression i)] -> Expression i

    -- | Apply function
    Apply :: Expression i -> [Expression i] -> Expression i

    Lambda -- ^ Lambda Calculus
        :: [Expression PatternContext]
        -- ^ Arguments pattern
        -> [Expression DeclarationContext]
        -- ^ Internal declarations
        -> Expression ExpressionContext
        -- ^ Expression body
        -> Expression ExpressionContext

    If -- ^ Basic if-expression
        :: Expression ExpressionContext
        -- ^ Condition expression
        -> Expression ExpressionContext
        -- ^ Returning expression when result of the condition is true
        -> Expression ExpressionContext
        -- ^ Returning expression when result of the condition is false
        -> Expression ExpressionContext

    MultiwayIf -- ^ Multiway conditional branch if-expression
        :: [(Expression ExpressionContext, Expression ExpressionContext)]
        -- ^ Conditional branches
        -> Expression ExpressionContext
        -- ^ Otherwise case
        -> Expression ExpressionContext

    Case -- ^ Pattern match conditional branch
        :: Expression ExpressionContext
        -- ^ Subject expression
        -> [(Expression PatternContext, Expression ExpressionContext)]
        -- ^ Pattern matching
        -> Expression ExpressionContext
        -- ^ Otherwise case
        -> Expression ExpressionContext

    Declare -- ^ Declare Type binding
        :: Expression DeclarationContext
        -> Expression TypingContext
        -> Expression DeclarationContext

    DefineConstant -- ^ Declare constant values
        :: Expression PatternContext
        -> Expression ExpressionContext
        -> Expression DeclarationContext

    DefineFunction -- ^ Declare function
        :: Expression PatternContext
        -> [Expression DeclarationContext]
        -> Expression ExpressionContext
        -> Expression DeclarationContext

deriving instance forall (i :: ContextIndex). Eq (Expression i)
deriving instance forall (i :: ContextIndex). Show (Expression i)
deriving instance forall (i :: ContextIndex). Typeable (Expression i)

data ExpressionF (f :: ContextIndex -> *) (i :: ContextIndex) where
    TermF :: Term -> Proxy i -> ExpressionF f i

    ListF :: [f i] -> ExpressionF f i

    KeyValueF :: [(String, f i)] -> ExpressionF f i

    ApplyF :: f i -> [f i] -> ExpressionF f i

    LambdaF
        :: [f PatternContext]
        -> [f DeclarationContext]
        -> f ExpressionContext
        -> ExpressionF f ExpressionContext

    IfF
        :: f ExpressionContext
        -> f ExpressionContext
        -> f ExpressionContext
        -> ExpressionF f ExpressionContext

    MultiwayIfF
        :: [(f ExpressionContext, f ExpressionContext)]
        -> f ExpressionContext
        -> ExpressionF f ExpressionContext

    CaseF
        :: f ExpressionContext
        -> [(f PatternContext, f ExpressionContext)]
        -> f ExpressionContext
        -> ExpressionF f ExpressionContext

    DeclareF
        :: f DeclarationContext
        -> f TypingContext
        -> ExpressionF f DeclarationContext

    DefineConstantF
        :: f PatternContext
        -> f ExpressionContext
        -> ExpressionF f DeclarationContext

    DefineFunctionF
        :: f PatternContext
        -> [f DeclarationContext]
        -> f ExpressionContext
        -> ExpressionF f DeclarationContext

deriving instance forall (f :: ContextIndex -> *) (i :: ContextIndex).
    ( Show (f ExpressionContext)
    , Show (f DeclarationContext)
    , Show (f PatternContext)
    , Show (f TypingContext)
    , Show (f i)
    ) => Show (ExpressionF f i)

deriving instance forall (f :: ContextIndex -> *) (i :: ContextIndex).
    ( Eq (f ExpressionContext)
    , Eq (f DeclarationContext)
    , Eq (f PatternContext)
    , Eq (f TypingContext)
    , Eq (f i)
    ) => Eq (ExpressionF f i)

instance IFunctor ExpressionF where
    ifmap _ (TermF t i)        = TermF t i
    ifmap f (ListF es)         = ListF $ f <$> es
    ifmap f (KeyValueF kv)     = KeyValueF $ fmap f <$> kv
    ifmap f (ApplyF fn as)     = ApplyF (f fn) $ f <$> as
    ifmap f (LambdaF as ds e)  = LambdaF (f <$> as) (f <$> ds) $ f e
    ifmap f (IfF c t e)        = IfF (f c) (f t) $ f e
    ifmap f (MultiwayIfF cs o) = MultiwayIfF ((f . fst &&& f . snd) <$> cs) $ f o
    ifmap f (CaseF t ps o)     = CaseF (f t) ((f . fst &&& f . snd) <$> ps) $ f o
    ifmap f (DeclareF t e)     = DeclareF (f t) $ f  e
    ifmap f (DefineConstantF v e)    = DefineConstantF (f  v) $ f  e
    ifmap f (DefineFunctionF p ss r) = DefineFunctionF (f p) (f <$> ss) $ f r

type instance IBase Expression = ExpressionF

instance IRecursive Expression where
    iproject (Term t i)              = TermF t i
    iproject (List es)               = ListF es
    iproject (KeyValue kv)           = KeyValueF kv
    iproject (Apply fn args)         = ApplyF fn args
    iproject (Lambda args ds e)      = LambdaF args ds e
    iproject (If c t e)              = IfF c t e
    iproject (MultiwayIf ps o)       = MultiwayIfF ps o
    iproject (Declare t e)           = DeclareF t e
    iproject (DefineConstant v e)    = DefineConstantF v e
    iproject (DefineFunction p ss r) = DefineFunctionF p ss r

instance ICorecursive Expression where
    iembed (TermF t i)              = Term t i
    iembed (ListF es)               = List es
    iembed (KeyValueF kv)           = KeyValue kv
    iembed (ApplyF fn as)           = Apply fn as
    iembed (LambdaF as ds e)        = Lambda as ds e
    iembed (IfF c t e)              = If c t e
    iembed (MultiwayIfF ps o)       = MultiwayIf ps o
    iembed (DeclareF t e)           = Declare t e
    iembed (DefineConstantF v e)    = DefineConstant v e
    iembed (DefineFunctionF p ss r) = DefineFunction p ss r

instance Pretty (Expression i) where
    pretty = icata' alg
      where
        alg :: ExpressionF (Const (Doc ann)) ~>. Doc ann
        alg (TermF t _) = pretty t

        alg (ListF es) = nestable brackets $ sep $ getConst <$> es

        alg (KeyValueF kv) = nestable braces $ sep $ prettyPair <$> kv

        alg (ApplyF (Const fn) args) = nestable parens $
            (fn <+>) $ sep $ getConst <$> args

        alg (LambdaF args decls (Const ret)) = nestable parens $
            "|" <> sep (getConst <$> args) <> "|" <+> sep (getConst <$> decls) <+> ret

        alg (IfF (Const c) (Const t) (Const e)) = nestable parens $
            "if" <+> sep [c, t, e]

        alg (MultiwayIfF cs (Const o)) = nestable parens $
            ("if" <+>) $ sep $ (casePair <$> cs) ++ [prettyOtherWise o]

        alg (CaseF (Const t) ps (Const o)) = nestable parens $
            ("case" <+> t <+>) $ sep $ (casePair <$> ps) ++ [prettyOtherWise o]

        alg (DeclareF (Const f) (Const t)) = nestable parens $
            ":" <+> f <+> t

        alg (DefineConstantF (Const c) (Const e)) = nestable parens $
            "=" <+> c <+> e

        alg (DefineFunctionF (Const p) decls (Const body)) = nestable parens $
            ("=" <+> p <+>) $ sep $ (getConst <$> decls) ++ [body]

        nestable :: (Doc ann -> Doc ann) -> Doc ann -> Doc ann
        nestable closing f = nesting $ \l -> indent l $ closing $ indent 4 f

        prettyPair :: forall ann j. (String, Const (Doc ann) j) -> Doc ann
        prettyPair (key, value) = ":" <> pretty key <+> getConst value

        casePair :: forall ann j k. (Const (Doc ann) j, Const (Doc ann) k) -> Doc ann
        casePair (Const pat, Const ret) = brackets $ indent 4 $ sep [pat, ret]

        prettyOtherWise :: Doc ann -> Doc ann
        prettyOtherWise o = brackets $ indent 4 $ sep ["otherwise", o]

expressionDepth :: Expression i -> Int
expressionDepth = icata' alg where
    alg :: ExpressionF (Const Int) ~>. Int
    alg (TermF _ _)             = 1
    alg (ListF xs)              = (1 +) $ maximum $ getConst <$> xs
    alg (KeyValueF kv)          = (1 +) $ maximum $ (snd >>> getConst) <$> kv
    alg (ApplyF (Const f) args) = (1 +) $ max f $ maximum $ getConst <$> args

