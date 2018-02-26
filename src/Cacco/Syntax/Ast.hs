{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cacco.Syntax.Ast where
--
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)
--
import           Data.IxAnn
import           Data.IxFix
--
import           Cacco.Syntax.Literal (Literal)

data Var
  = VarId String
  | ConId String
  | VarSym String
  | ConSym String
  deriving (Eq, Ord, Show, Typeable, Generic)

data AstIx
  = AstDecl
  | AstExpr
  | AstPatt
  | AstType
  deriving (Eq, Ord, Show, Typeable, Generic)

type AstDecl = 'AstDecl
type AstExpr = 'AstExpr
type AstPatt = 'AstPatt
type AstType = 'AstType

data AstIxProxy (i :: AstIx) where
  DeclProxy :: AstIxProxy 'AstDecl
  ExprProxy :: AstIxProxy 'AstExpr
  PattProxy :: AstIxProxy 'AstPatt
  TypeProxy :: AstIxProxy 'AstType

deriving instance Show (AstIxProxy i)
deriving instance Eq (AstIxProxy i)
deriving instance Ord (AstIxProxy i)
deriving instance Typeable (AstIxProxy i)
--
data AstF (f :: AstIx -> *) (i :: AstIx) where
  -- Atomic (Leaf Node of AST)
  -- | Hole @_@
  HolF :: AstF f AstPatt
  -- | 'Literal'
  LitF :: Literal -> AstIxProxy i -> AstF f i
  -- | Variable
  VarF :: Var -> AstIxProxy i -> AstF f i

  -- Collections
  -- | Linked list
  LisF :: [f i] -> AstF f i

  -- Fuctors
  -- | if
  IfF :: f AstExpr -> f AstExpr -> f AstExpr -> AstF f AstExpr
  -- | multyway if
  MifF :: [(f AstExpr, f AstExpr)] -> f AstExpr -> AstF f AstExpr
  -- | Apply function
  AppF :: f i -> [f i] -> AstF f i
  -- | Lambda (anonymous) function
  LamF :: [f AstPatt] -> f AstExpr -> AstF f AstExpr

  -- Declarations
  -- | Declare type bind
  DecF :: f AstPatt -> f AstType -> AstF f AstDecl
  -- | Define function or constants
  DefF :: f AstPatt -> f AstExpr -> AstF f AstDecl
--
deriving instance forall (f :: AstIx -> *) (i :: AstIx).
  ( Show (f AstExpr)
  , Show (f AstPatt)
  , Show (f AstDecl)
  , Show (f AstType)
  , Show (f i)
  ) => Show (AstF f i)
--
deriving instance forall (f :: AstIx -> *) (i :: AstIx).
  ( Eq (f AstExpr)
  , Eq (f AstPatt)
  , Eq (f AstDecl)
  , Eq (f AstType)
  , Eq (f i)
  ) => Eq (AstF f i)
--
deriving instance forall (f :: AstIx -> *) (i :: AstIx).
  ( Ord (f AstExpr)
  , Ord (f AstPatt)
  , Ord (f AstDecl)
  , Ord (f AstType)
  , Ord (f i)
  ) => Ord (AstF f i)
--
deriving instance forall (f :: AstIx -> *) (i :: AstIx).
  ( Typeable (f AstExpr)
  , Typeable (f AstPatt)
  , Typeable (f AstDecl)
  , Typeable (f AstType)
  , Typeable (f i)
  ) => Typeable (AstF f i)
--
instance IxFunctor AstF where
  imap = imapDefault

instance IxFoldable AstF where
  iFoldMap = iFoldMapDefault

instance IxTraversable AstF where
  itraverse f ast = case ast of
      HolF         -> pure HolF
      VarF v i     -> VarF <$> pure v <*> pure i
      LitF v i     -> LitF <$> pure v <*> pure i
      LisF elems   -> LisF <$> mapList f elems
      AppF fn args -> AppF <$> f fn <*> mapList f args
      LamF ps expr -> LamF <$> mapList f ps <*> f expr
    where
      mapList fn xs = sequenceA $ fn <$> xs
--
type Ast = IxFix AstF

pattern Var :: forall (i :: AstIx).() => Var -> AstIxProxy i -> Ast i
pattern Var v i = In (VarF v i)

pattern Lit :: forall (i :: AstIx).() => Literal -> AstIxProxy i -> Ast i
pattern Lit l i = In (LitF l i)

pattern Lis :: forall (i :: AstIx).() => [Ast i] -> Ast i
pattern Lis elements = In (LisF elements)

pattern App :: forall (i :: AstIx).() => Ast i -> [Ast i] -> Ast i
pattern App fn args = In (AppF fn args)

pattern Lam :: forall (i :: AstIx).() => i ~ AstExpr => [Ast AstPatt] -> Ast AstExpr -> Ast i
pattern Lam pt body = In (LamF pt body)

pattern Hole :: forall (i :: AstIx).() => i ~ AstPatt => Ast i
pattern Hole = In HolF

type AnnAst a i = IxAnn a AstF i
