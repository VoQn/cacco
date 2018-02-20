
module Cacco.Eval where

import           Control.Monad          (sequence)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import           Cacco.Ann              (unAnnF)
import           Cacco.Core             ()
import           Cacco.Env              (Env)
import qualified Cacco.Env              as Env
import           Cacco.Error            (Error (..))
import qualified Cacco.Error            as Error
import           Cacco.Fix              (cata)
import           Cacco.Syntax.Expr      (AstF (..), Expr)
import qualified Cacco.Syntax.Literal   as Lit
import           Cacco.Val              (Val (..))
import qualified Cacco.Val              as Val

type Eval i a = ReaderT (Env a) (ExceptT (Error i) (StateT (Env a) Identity)) a

runEval :: Eval i a -> Env a -> (Either (Error i) a, Env a)
runEval ev env = runIdentity $ runStateT (runExceptT $ runReaderT ev env) env

type EvalF i = Eval i (Val i)

evalAcc :: (i, AstF (EvalF i)) -> EvalF i
evalAcc (i, LitF l) = case l of
  Lit.Undef     -> throwError $ Message "undefined" (Just i)
  Lit.Unit      -> return $ Unit      $ Just i
  Lit.Bool    x -> return $ Bool    x $ Just i
  Lit.Int8    x -> return $ Int8    x $ Just i
  Lit.Int16   x -> return $ Int16   x $ Just i
  Lit.Int32   x -> return $ Int32   x $ Just i
  Lit.Int64   x -> return $ Int64   x $ Just i
  Lit.Uint8   x -> return $ Uint8   x $ Just i
  Lit.Uint16  x -> return $ Uint16  x $ Just i
  Lit.Uint32  x -> return $ Uint32  x $ Just i
  Lit.Uint64  x -> return $ Uint64  x $ Just i
  Lit.Integer x -> return $ Integer x $ Just i
  Lit.Float16 x -> return $ Float16 x $ Just i
  Lit.Float32 x -> return $ Float32 x $ Just i
  Lit.Float64 x -> return $ Float64 x $ Just i
  Lit.Flonum  x -> return $ Flonum  x $ Just i
  Lit.Text    x -> return $ Text    x $ Just i

evalAcc (i, SymF s) = do
  result <- do env <- ask; return (Env.get env s)
  case result of
    Left err
      | Error.hasInfo err -> throwError err
      | otherwise -> throwError $ Error.setInfo (Just i) err
    Right val -> return $ Val.setInfo (Just i) val

evalAcc (i, ConF s expr) = do
  env <- ask
  value <- expr
  newEnv <- Env.push env s value
  put newEnv
  return $ Unit $ Just i

evalAcc (i, AppF fn args) = do
  res <- fn
  case res of
    Builtin f _ -> do
      vs <- sequence args
      case f vs of
        Left err
          | Error.hasInfo err -> throwError err
          | otherwise -> throwError $ Error.setInfo (Just i) err
        Right result -> return $ Val.setInfo (Just i) result

evalAcc (i, _) = throwError $ InvalidForm $ Just i

eval :: Env (Val i) -> Expr i -> (Either (Error i) (Val i), Env (Val i))
eval env expr = runEval (cata (evalAcc . unAnnF) expr) env
