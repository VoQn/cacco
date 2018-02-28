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

module Cacco.Eval2 where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map               as Map
import           Data.Typeable          (Typeable)

import           Cacco.Syntax.AST
import           Cacco.Syntax.Literal   (Literal)
import qualified Cacco.Syntax.Literal   as Literal
import           Data.IxAnn             ()
import           Data.IxFix             ()


-- Value
data Val
  = Unit
  | Symbol  String
  | Integer Integer
  | Bool    Bool
  deriving (Eq, Show, Typeable)

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
initEnv = Env { symbolTable = Map.empty, funcTable = Map.empty }

findSymbol :: String -> Env -> Maybe Val
findSymbol key = Map.lookup key . symbolTable

hasSymbol :: String -> Env -> Bool
hasSymbol key = Map.member key . symbolTable

registerSymbol :: String -> Val -> Env -> Env
registerSymbol key val env@Env{..} =
  let
    newTable = Map.insert key val symbolTable
  in
    env{ symbolTable = newTable }

registerFunc :: String -> Fun -> Env -> Env
registerFunc name func env@Env{..} =
  let
    newSymbolTable = Map.insert name Unit symbolTable
    newFuncTable   = Map.insert name func funcTable
  in
    env{ symbolTable = newSymbolTable, funcTable = newFuncTable }


-- Evaluator
type Eval = ExceptT String (StateT Env Identity) Val

builtinEnv :: Env
builtinEnv = initEnv

runEval :: Eval -> Env -> (Either String Val, Env)
runEval ev = runIdentity . runStateT (runExceptT ev)

evalLit :: Literal -> Val
evalLit lit = case lit of
  Literal.Integer x -> Integer x
  Literal.Bool    x -> Bool x
  _                 -> Bool False

astExpr' :: ((f AstExpr) -> Eval) -> AstF f AstExpr -> Eval
astExpr' ee (IfF c t e) = do
  value <- ee c
  case value of
    Bool flag
      | flag      -> ee t
      | otherwise -> ee e
    _         -> throwError "Non-boolean value evaluated as condition in if-expression"

astDecl :: (Ast AstExpr -> Eval) -> Ast AstDecl -> Eval
astDecl ee (Def p e) = case p of
  -- constant
  Var (VarSym name) _ -> do
    env <- get
    if hasSymbol name env
      then throwError $ "Can not redefine symbol '" ++ name ++ "'."
      else do
        value <- ee e
        put $ registerSymbol name value env
        return Unit
  -- function
  App (Var (VarSym name) _) args -> do
    env <- get
    if hasSymbol name env
      then throwError $ "Can not redefine symbol '" ++ name ++ "'."
      else do
        let fn = Fun { funcName = Just name, localEnv = env, funcArgs = args, funcBody = e }
        put $ registerFunc name fn env
        return Unit

evalAST :: forall (i :: AstIx). Ast i -> Eval
evalAST (Lit l _)  = return $ evalLit l

evalAST (Var (VarSym name) proxy) = case proxy of
  DeclProxy -> return $ Symbol name
  PattProxy -> return $ Symbol name
  TypeProxy -> return $ Symbol name
  ExprProxy -> do
    env <- get
    case findSymbol name env of
      Nothing -> throwError $ "Undefined symbol '" ++ name ++ "' referenced."
      Just  v -> return v
