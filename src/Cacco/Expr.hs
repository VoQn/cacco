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
import           Cacco.Literal    (Literal)
import qualified Cacco.Literal    as Lit
import           Data.Map         (Map)
import           Data.Monoid      ((<>))
import           Data.Traversable
import           Data.Typeable    (Typeable)
import           GHC.Generics     (Generic)

data ExprF a
  = HolF
  | LitF Lit.Literal
  -- | Symbol
  | SymF String
  -- | Linked list
  | LisF [a]
  -- | Fixed size vector
  | VecF [a]
  -- | Struct
  | StrF (Map String a)
  -- | Apply function
  | AppF a [a]
  -- | Lambda (anonymous) function
  | LamF [a] a
  deriving (Eq, Ord, Show, Typeable, Generic, Functor)

type Expr = Fix ExprF
pattern Hole :: Expr
pattern Hole = Fix (HolF)

pattern Literal :: Literal -> Expr
pattern Literal lit = Fix (LitF lit)

pattern Symbol :: String -> Expr
pattern Symbol sym = Fix (SymF sym)

pattern List :: [Expr] -> Expr
pattern List vals = Fix (LisF vals)

pattern Vector :: [Expr] -> Expr
pattern Vector vals = Fix (VecF vals)

pattern Struct :: Map String Expr -> Expr
pattern Struct map = Fix (StrF map)

pattern App :: Expr -> [Expr] -> Expr
pattern App fn args = Fix (AppF fn args)

pattern Lam :: [Expr] -> Expr -> Expr
pattern Lam params body = Fix (LamF params body)

instance Foldable ExprF where
  foldMap = foldMapDefault

instance Traversable ExprF where
  traverse _ HolF     = pure HolF
  traverse _ (SymF v) = SymF <$> pure v
  traverse _ (LitF l) = LitF <$> pure l

  traverse f expr = case expr of
      LisF elements    -> LisF <$> mapList f elements
      VecF elements    -> VecF <$> mapList f elements
      AppF func args   -> AppF <$> f func <*> mapList f args
      LamF params body -> LamF <$> mapList f params <*> f body
    where
      mapList fn xs = sequenceA $ fn <$> xs

data Info i f a = Info
  {
    info    :: i,
    content :: f a
  } deriving (Eq, Show, Typeable, Generic)

instance Functor f => Functor (Info i f) where
  fmap f (Info i c) = (Info i) $ f <$> c

instance (Functor f, Traversable f) => Foldable (Info i f) where
  foldMap = foldMapDefault

instance (Functor f, Traversable f) => Traversable (Info i f) where
  traverse f (Info i c) = (Info i) <$> traverse f c

type Annotated i = Fix (Info i ExprF)

cata :: Functor f
  => (f a -> a)
  -> (Fix f -> a)
cata phi = phi . fmap (cata phi) . out

removeAnn :: Annotated i -> Expr
removeAnn (Fix (Info _ expr)) = rm expr
  where
    rm HolF               = Hole
    rm (LitF literal)     = Literal literal
    rm (SymF symbol)      = Symbol symbol
    rm (LisF elements)    = List $ removeAnn <$> elements
    rm (VecF elements)    = Vector $ removeAnn <$> elements
    rm (AppF fn args)     = App (removeAnn fn) (removeAnn <$> args)
    rm (LamF params body) = Fix $ LamF (removeAnn <$> params) (removeAnn body)
--
prettyfy :: Expr -> String
prettyfy Hole = "_"
prettyfy (Symbol s) = s
prettyfy (Literal l) = case l of
  Lit.Undefined     -> "undefined"
  Lit.Boolean True  -> "true"
  Lit.Boolean False -> "false"
  Lit.Integer x     -> show x
  Lit.Flonum x      -> show x
  _                 -> undefined
prettyfy (List elems) =
  let
    es = prettyfy <$> elems
    oneline = unwords es
  in "`(" <> oneline <> ")"

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
