{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
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

module Cacco.Ast where

import           Cacco.IxFix      (IxFix (..), IxFunctor (..),
                                   IxTraversable (..), cata, imapDefault)
import           Data.Traversable ()

import           Cacco.Literal    (Literal)

data Var
  = VarId String
  | ConId String
  | VarSym String
  | ConSym String
  deriving (Eq, Ord, Show)

data AstIx
  = AstDecl
  | AstExpr
  | AstPatt
  | AstType
  -- deriving (Eq, Show)

type AstDecl = 'AstDecl
type AstExpr = 'AstExpr
type AstPatt = 'AstPatt
type AstType = 'AstType

data AstIxProxy (i :: AstIx) where
  DeclProxy :: AstIxProxy AstDecl
  ExprProxy :: AstIxProxy AstExpr
  PattProxy :: AstIxProxy AstPatt
  TypeProxy :: AstIxProxy AstType

data AstF (f :: AstIx -> *) (i :: AstIx) where
  VarF :: Var -> AstIxProxy i -> AstF f i
  LitF :: Literal -> AstF f AstExpr
  LisF :: [f AstExpr] -> AstF f AstExpr
  AppF :: f i -> [f i] -> AstF f i
  LamF :: [f AstPatt] -> f AstExpr -> AstF f AstExpr
  HoleF :: AstF f AstPatt

type Ast = IxFix AstF

pattern Var :: forall (i :: AstIx).
               Var          -- ^ variable identifier
            -> AstIxProxy i -- ^ index proxy
            -> Ast i
pattern Var v i = In (VarF v i)

pattern Lit :: forall (i :: AstIx). ()
            => i ~ AstExpr
            => Literal   -- ^ concrete literal
            -> Ast i
pattern Lit l = In (LitF l)

pattern Lis :: forall (i :: AstIx). ()
            => i ~ AstExpr
            => [Ast AstExpr]
            -> Ast i
pattern Lis elements = In (LisF elements)

pattern App :: forall (i :: AstIx).
               Ast i   -- ^ function
            -> [Ast i] -- ^ arguments
            -> Ast i
pattern App fn args = In (AppF fn args)

pattern Lam :: forall (i :: AstIx).()
            => i ~ AstExpr
            => [Ast AstPatt] -- ^ parameters pattern
            -> Ast AstExpr   -- ^ body expression
            -> Ast i
pattern Lam patterns expr = In (LamF patterns expr)

pattern Hole :: forall (i :: AstIx).()
             => i ~ AstPatt
             => Ast i
pattern Hole = In HoleF

newtype AnnAstF an fn ix = Ann { unAnn :: (an, AstF fn ix) }

instance IxFunctor AstF where
  imap = imapDefault

instance IxFunctor (AnnAstF x) where
  imap = imapDefault

instance IxTraversable AstF where
  itraverse _ (VarF v i) = VarF <$> pure v <*> pure i
  itraverse _ (LitF l)   = LitF <$> pure l
  itraverse _ HoleF      = pure HoleF

  itraverse f ast = case ast of
      LisF elems   -> LisF <$> mapList f elems
      AppF fn args -> AppF <$> f fn <*> mapList f args
      LamF ps expr -> LamF <$> mapList f ps <*> f expr
    where
      mapList fn xs = sequenceA $ fn <$> xs

instance IxTraversable (AnnAstF x) where
  itraverse f (Ann (x, t)) = Ann . (,) x <$> itraverse f t

removeAnn :: IxFix (AnnAstF a) i -> IxFix AstF i
removeAnn = cata $ In . snd . unAnn
