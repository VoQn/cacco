{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Cacco.Syntax.AST.Indexed where
--
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)
--
import           Data.IxAnn
import           Data.IxFix
--
import           Cacco.Syntax.Literal (Literal)

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
  DeclProxy :: AstIxProxy AstDecl
  ExprProxy :: AstIxProxy AstExpr
  PattProxy :: AstIxProxy AstPatt
  TypeProxy :: AstIxProxy AstType

deriving instance Show (AstIxProxy i)
deriving instance Eq (AstIxProxy i)
deriving instance Ord (AstIxProxy i)
deriving instance Typeable (AstIxProxy i)
--
data AstF (r :: AstIx -> *) (i :: AstIx) where
  -- Atomic (Leaf Node of AST)
  -- | Hole @_@
  HoleF :: AstF r AstPatt
  -- | 3 Dots @...@
  DotsF :: AstF r AstPatt
  -- | 'Literal'
  LitF :: Literal -> AstIxProxy i -> AstF r i
  -- | Variable
  VarF :: String -> AstIxProxy i -> AstF r i

  -- Collections
  -- | Linked list
  LisF :: [r i] -> AstF r i

  -- Fuctors
  -- | if
  IfF  :: r AstExpr -> r AstExpr -> r AstExpr -> AstF r AstExpr
  -- | multyway if
  MifF :: [(r AstExpr, r AstExpr)] -> r AstExpr -> AstF r AstExpr
  -- | Apply function
  AppF :: r i -> [r i] -> AstF r i
  -- | Lambda (anonymous) function
  LamF :: [r AstPatt] -> r AstExpr -> AstF r AstExpr

  -- Declarations
  -- | Declare type bind
  DecF :: String -> [r AstType] -> AstF r AstDecl
  -- | Define function or constants
  DefF :: r AstPatt -> r AstExpr -> AstF r AstDecl

deriving instance forall (r :: AstIx -> *) (i :: AstIx).
  ( Show (r AstExpr)
  , Show (r AstPatt)
  , Show (r AstDecl)
  , Show (r AstType)
  , Show (r i)
  ) => Show (AstF r i)
--
deriving instance forall (r :: AstIx -> *) (i :: AstIx).
  ( Eq (r AstExpr)
  , Eq (r AstPatt)
  , Eq (r AstDecl)
  , Eq (r AstType)
  , Eq (r i)
  ) => Eq (AstF r i)
--
deriving instance forall (r :: AstIx -> *) (i :: AstIx).
  ( Typeable (r AstExpr)
  , Typeable (r AstPatt)
  , Typeable (r AstDecl)
  , Typeable (r AstType)
  , Typeable (r i)
  ) => Typeable (AstF r i)
--
instance IxFunctor AstF where
  imap = imapDefault

instance IxFoldable AstF where
  ifoldMap = ifoldMapDefault

instance IxTraversable AstF where
  itraverse f ast = case ast of
      HoleF        -> pure HoleF
      DotsF        -> pure DotsF
      VarF v i     -> VarF <$> pure v <*> pure i
      LitF v i     -> LitF <$> pure v <*> pure i
      LisF vs      -> LisF <$> mapList f vs
      IfF  c t e   -> IfF  <$> f c  <*> f t <*> f e
      AppF fn args -> AppF <$> f fn <*> mapList f args
      LamF ps expr -> LamF <$> mapList f ps <*> f expr
      DecF v ts    -> DecF <$> pure v <*> mapList f ts
      DefF n v     -> DefF <$> f n <*> f v
    where
      mapList fn xs = sequenceA $ fn <$> xs
--
type Ast = IxFix AstF

pattern Hole :: forall (i :: AstIx). i ~ AstPatt => Ast i
pattern Hole = In HoleF

pattern Dots :: forall (i :: AstIx). i ~ AstPatt => Ast i
pattern Dots = In DotsF

pattern Var :: forall (i :: AstIx). String -> AstIxProxy i -> Ast i
pattern Var v i = In (VarF v i)

pattern Lit :: forall (i :: AstIx). Literal -> AstIxProxy i -> Ast i
pattern Lit v i = In (LitF v i)

pattern Lis :: forall (i :: AstIx). [Ast i] -> Ast i
pattern Lis vs = In (LisF vs)

pattern App :: forall (i :: AstIx). Ast i -> [Ast i] -> Ast i
pattern App fn args = In (AppF fn args)

pattern Lam :: forall (i :: AstIx). i ~ AstExpr => [Ast AstPatt] -> Ast AstExpr -> Ast i
pattern Lam pt body = In (LamF pt body)

pattern Def :: forall (i :: AstIx). i ~ AstDecl => Ast AstPatt -> Ast AstExpr -> Ast i
pattern Def n v = In (DefF n v)

pattern Dec :: forall (i :: AstIx). i ~ AstDecl => String -> [Ast AstType] -> Ast i
pattern Dec n t = In (DecF n t)

pattern If :: forall (i :: AstIx). i ~ AstExpr => Ast i -> Ast i -> Ast i -> Ast i
pattern If c t e = In (IfF c t e)

type AnnAst a i = IxAnn a AstF i
