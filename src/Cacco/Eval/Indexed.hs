{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Cacco.Eval.Indexed where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Functor.Const       (Const (..))
import qualified Data.Map                 as Map
import           Data.Typeable            (Typeable)

import           Cacco.Syntax.AST.Indexed
import           Cacco.Syntax.Literal     (Literal)
import qualified Cacco.Syntax.Literal     as Literal
import           Data.IxAnn               ()
import           Data.IxFix               (cata')

-- Value
data Val
  = Unit
  | Symbol  String
  | Integer Integer
  | Bool    Bool
  | Abs     (Maybe String) [Maybe String]
  deriving (Eq, Show, Typeable)

type Fn e v = [v] -> Either e v

data Fun = Fun {
    funcName :: Maybe String,
    funcArgs :: [Ast AstPatt],
    localEnv :: Env,
    funcBody :: Ast AstExpr
  }

-- Environment
data Env = Env {
    symbolTable :: Map.Map String Val,
    funcTable   :: Map.Map String Fun
  }

initEnv :: Env
initEnv = Env {
    symbolTable = Map.empty,
    funcTable = Map.empty
  }

findSymbol :: String -> Env -> Maybe Val
findSymbol key = Map.lookup key . symbolTable

hasSymbol :: String -> Env -> Bool
hasSymbol key = Map.member key . symbolTable

registerSymbol :: String -> Val -> Env -> Env
registerSymbol key val env@Env{..} =
    let
      newTable = Map.insert key val symbolTable
    in
      env { symbolTable = newTable }

registerFunc :: String -> Fun -> Env -> Env
registerFunc name func env@Env{..} =
    env { symbolTable = newSymbolTable, funcTable = newFuncTable }
  where
    newSymbolTable = Map.insert name Unit symbolTable
    newFuncTable   = Map.insert name func funcTable

-- Evaluator
type EvalT e s m a = ExceptT e (StateT s m) a
type Eval a = EvalT String Env Identity a

runEvalT :: EvalT e s m a -> s -> m (Either e a, s)
runEvalT ev = runStateT (runExceptT ev)

runEval :: EvalT e s Identity a -> s -> (Either e a, s)
runEval ev = runIdentity . runEvalT ev

builtinEnv :: Env
builtinEnv = initEnv

eval :: forall (i :: AstIx). Ast i -> Env -> (Either String Val, Env)
eval expr = runEval eval'
  where
    eval' :: Eval Val
    eval' = cata' alg $ expr
    {-# INLINE eval' #-}

alg :: forall (i :: AstIx). AstF (Const (Eval Val)) i -> Eval Val
alg (LitF l _) = evalLit l
alg (VarF v p) = evalVar v p
alg (IfF (Const c) (Const t) (Const e)) = do
    cond <- c
    case cond of
      Bool True  -> t
      Bool False -> e
      _ -> throwError "Non-boolean value evaluated as condition in if-expression"

alg (AppF (Const _f') _args') = do
    -- f <- f'
    -- case f of
    --   Symbol name -> do
    --     env <- get
    --     case findFunc name env of
    --       Nothing ->
    --     _args <- sequence $ getConst <$> args'
    return Unit

alg (DefF (Const patt') (Const expr')) = do
    patt <- patt'
    case patt of
      _ -> expr'

evalLit :: Literal -> Eval Val
evalLit lit = case lit of
    Literal.Integer x -> return $ Integer x
    Literal.Bool    x -> return $ Bool x
    _                 -> return $ Bool False

evalVar :: String -> AstIxProxy i -> Eval Val
evalVar name proxy = case proxy of
    DeclProxy -> return $ Symbol name
    PattProxy -> return $ Symbol name
    TypeProxy -> return $ Symbol name
    ExprProxy -> do
      env <- get
      case findSymbol name env of
        Nothing -> throwError $ "Undefined symbol '" ++ name ++ "' referenced."
        Just  v -> return v

-- evalDefConst :: String -> Ast AstExpr -> Eval Val
-- evalDefConst name expr = do
--     env <- get
--     if hasSymbol name env
--       then throwError $ "Can not redefine symbol '" ++ name ++ "'."
--       else do
--         value <- evalExpr expr
--         put $ registerSymbol name value env
--         return Unit

-- evalDefFunc :: String -> [Ast AstPatt] -> Ast AstExpr -> Eval Val
-- evalDefFunc name args body = do
--     env <- get
--     if hasSymbol name env
--       then throwError $ "Can not redefine symbol '" ++ name ++ "'."
--       else do
--         let fn = Fun {
--           funcName = Just name,
--           localEnv = env,
--           funcArgs = args,
--           funcBody = body
--         }
--         put $ registerFunc name fn env
--         return Unit
