{-# LANGUAGE DataKinds            #-}
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

import           Cacco.Fix
import           Cacco.Literal    (Literal)
import qualified Cacco.Literal    as Lit
import           Data.Map         (Map)
import           Data.Monoid      ((<>))
import           Data.Traversable
import           Data.Typeable    (Typeable)
import           GHC.Generics     (Generic)

data AstF a
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

instance Foldable AstF where
  foldMap = foldMapDefault

instance Traversable AstF where
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
  } deriving (Eq, Ord, Show, Typeable, Generic)

instance Functor f => Functor (Info i f) where
  fmap f (Info i c) = Info i $ f <$> c

instance (Functor f, Traversable f) => Foldable (Info i f) where
  foldMap = foldMapDefault

instance (Functor f, Traversable f) => Traversable (Info i f) where
  traverse f (Info i c) = Info i <$> traverse f c

newtype Annotated i a = Ann { unAnn :: Info i AstF a }
  deriving (Eq, Ord, Show, Typeable, Generic, Functor)

type Expr i = Fix (Annotated i)

removeAnn :: Expr i -> Ast
removeAnn = cata $ Fix . content . unAnn

--

prettyfy :: Ast -> String
prettyfy Hole = "_"
prettyfy (Symbol s) = s
prettyfy (Literal l) = case l of
  Lit.Undef      -> "undefined"
  Lit.Bool True  -> "true"
  Lit.Bool False -> "false"
  Lit.Integer x  -> show x
  Lit.Flonum x   -> show x
  _              -> undefined
prettyfy (List elems) =
  let
    es = prettyfy <$> elems
    oneline = unwords es
  in "`(" <> oneline <> ")"

isAtomic :: AstF a -> Bool
isAtomic (LisF _) = False
isAtomic (VecF _) = False
isAtomic (StrF _) = False
isAtomic _        = True

isCollection :: AstF a -> Bool
isCollection (LisF _) = True
isCollection (VecF _) = True
isCollection (StrF _) = True
isCollection _        = False
