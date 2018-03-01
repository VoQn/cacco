{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cacco.Eval where

import           Control.Monad          (sequence)
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State    (StateT, runStateT)
import qualified Control.Monad.State    as State

import           Data.Ann               (unAnnF)
import           Data.Fix               (cata)

import           Cacco.Env              (Env)
import qualified Cacco.Env              as Env
import           Cacco.Error            (Error (..))
import qualified Cacco.Error            as Err
import           Cacco.Syntax.Expr      (AstF (..), Expr)
import qualified Cacco.Syntax.Literal   as Lit
import           Cacco.Val              (Val (..), pretty)
import qualified Cacco.Val              as Val

type Eval i a = ExceptT (Error i) (StateT (Env a) Identity) a

runEval :: Eval i a -> Env a -> (Either (Error i) a, Env a)
runEval evaluator = runIdentity . runStateT (runExceptT evaluator)

type EvalF i = Eval i (Val i)
type EvalResult i = (Either (Error i) (Val i), Env (Val i))

supplyInfo :: MonadError (Error i) m => i -> Either (Error i) (Val i) -> m (Val i)
supplyInfo info result = case result of
    Left  err -> throwError $ Err.supplyInfo info err
    Right val -> return $ Val.setInfo (Just info) val

evalLit :: MonadError (Error i) m => Lit.Literal -> m (Val i)
evalLit l = case l of
    Lit.Undef     -> throwError $ Err.message "undefined"
    Lit.Unit      -> return Val.unit
    Lit.Bool    x -> return $ Val.bool    x
    Lit.Int8    x -> return $ Val.int8    x
    Lit.Int16   x -> return $ Val.int16   x
    Lit.Int32   x -> return $ Val.int32   x
    Lit.Int64   x -> return $ Val.int64   x
    Lit.Uint8   x -> return $ Val.uint8   x
    Lit.Uint16  x -> return $ Val.uint16  x
    Lit.Uint32  x -> return $ Val.uint32  x
    Lit.Uint64  x -> return $ Val.uint64  x
    Lit.Integer x -> return $ Val.integer x
    Lit.Float16 x -> return $ Val.float16 x
    Lit.Float32 x -> return $ Val.float32 x
    Lit.Float64 x -> return $ Val.float64 x
    Lit.Flonum  x -> return $ Val.flonum  x
    Lit.Text    x -> return $ Val.text    x

evalAcc :: forall i. (i, AstF (EvalF i)) -> EvalF i
evalAcc (info, LitF l) = supplyInfo info $ evalLit l

evalAcc (info, SymF name) = do
    env <- State.get
    supplyInfo info $ case Env.lookup name env of
      Nothing  -> return $ Symbol name Nothing
      Just val -> return val

evalAcc (info, ConF symE exprE) = do
    sym <- symE
    case sym of
      Symbol name _ -> do
        val <- exprE
        env <- State.get
        _ <- State.put (Env.register name val env)
        return . Unit $ Just info
      _ -> throwError $ CanNotRedefine "" $ Just info

evalAcc (info, AppF funcE argEs) = do
    val <- funcE
    args <- sequence argEs
    evalAsFunc val args
  where
    evalAsFunc (Builtin func _) = supplyInfo info . func
    evalAsFunc v = const . throwError $ CanNotCallAsFunction (pretty v) $ Just info

evalAcc (i, _) = throwError . InvalidForm $ Just i

eval :: Expr i -> Env (Val i) -> EvalResult i
eval expr = runEval $ cata (evalAcc . unAnnF) expr
