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
    let
      newSymbolTable = Map.insert name Unit symbolTable
      newFuncTable   = Map.insert name func funcTable
    in
      env { symbolTable = newSymbolTable, funcTable = newFuncTable }

-- Evaluator
type EvalT a = ExceptT String (StateT Env Identity) a

builtinEnv :: Env
builtinEnv = initEnv

runEval :: EvalT a -> Env -> (Either String a, Env)
runEval ev = runIdentity . runStateT (runExceptT ev)

evalExpr :: Ast AstExpr -> EvalT Val
evalExpr ast = case ast of
    Lit l _  -> evalLit l
    Var v p  -> evalVar v p
    If c t e -> evalIfSyntax c t e
    _        -> throwError "Have not implemented yet."


evalDecl :: Ast AstDecl -> EvalT Val
evalDecl ast = case ast of
    Lit l _ -> evalLit l
    Var v p -> evalVar v p
    Def p e -> evalDef p e
    _       -> throwError "Have not implemented yet."

evalLit :: Literal -> EvalT Val
evalLit lit = case lit of
    Literal.Integer x -> return $ Integer x
    Literal.Bool    x -> return $ Bool x
    _                 -> return $ Bool False

evalVar :: Var -> AstIxProxy i -> EvalT Val
evalVar (VarSym name) proxy = case proxy of
    DeclProxy -> return $ Symbol name
    PattProxy -> return $ Symbol name
    TypeProxy -> return $ Symbol name
    ExprProxy -> do
      env <- get
      case findSymbol name env of
        Nothing -> throwError $ "Undefined symbol '" ++ name ++ "' referenced."
        Just  v -> return v

evalIfSyntax :: Ast AstExpr -> Ast AstExpr -> Ast AstExpr -> EvalT Val
evalIfSyntax cond thenCase elseCase = do
    result <- evalExpr cond
    case result of
      Bool False -> evalExpr elseCase
      Bool True  -> evalExpr thenCase
      _ -> throwError "Non-boolean value evaluated as condition in if-expression"

evalDef :: Ast AstPatt -> Ast AstExpr -> EvalT Val
evalDef patt expr = case patt of
    -- constant
    Var (VarSym name) _            -> evalDefConst name expr
    -- function
    App (Var (VarSym name) _) args -> evalDefFunc name args expr

evalDefConst :: String -> Ast AstExpr -> EvalT Val
evalDefConst name expr = do
    env <- get
    if hasSymbol name env
      then throwError $ "Can not redefine symbol '" ++ name ++ "'."
      else do
        value <- evalExpr expr
        put $ registerSymbol name value env
        return Unit

evalDefFunc :: String -> [Ast AstPatt] -> Ast AstExpr -> EvalT Val
evalDefFunc name args body = do
    env <- get
    if hasSymbol name env
      then throwError $ "Can not redefine symbol '" ++ name ++ "'."
      else do
        let fn = Fun {
          funcName = Just name,
          localEnv = env,
          funcArgs = args,
          funcBody = body
        }
        put $ registerFunc name fn env
        return Unit
