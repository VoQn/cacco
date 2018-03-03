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
import           Data.Functor.Foldable
import           Data.Scientific        (toRealFloat)

import           Data.Ann               (unAnnF)

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
    Lit.Int8    x -> return $ Val.int8    $ fromInteger x
    Lit.Int16   x -> return $ Val.int16   $ fromInteger x
    Lit.Int32   x -> return $ Val.int32   $ fromInteger x
    Lit.Int64   x -> return $ Val.int64   $ fromInteger x
    Lit.Uint8   x -> return $ Val.uint8   $ fromIntegral x
    Lit.Uint16  x -> return $ Val.uint16  $ fromIntegral x
    Lit.Uint32  x -> return $ Val.uint32  $ fromIntegral x
    Lit.Uint64  x -> return $ Val.uint64  $ fromIntegral x
    Lit.Integer x -> return $ Val.integer x
    Lit.Numeric x -> return $ Val.natural x
    Lit.Float16 x -> return $ Val.float16 $ toRealFloat x
    Lit.Float32 x -> return $ Val.float32 $ toRealFloat x
    Lit.Float64 x -> return $ Val.float64 $ toRealFloat x
    Lit.Flonum  x -> return $ Val.flonum  x
    Lit.Text    x -> return $ Val.text    x

evalAcc :: forall i. (AstF (EvalF i), i) -> EvalF i
evalAcc (LitF l, info) = supplyInfo info $ evalLit l

evalAcc (SymF name, info) = do
    env <- State.get
    supplyInfo info $ case Env.lookup name env of
      Nothing  -> return $ Symbol name Nothing
      Just val -> return val

evalAcc (ConF symE exprE, info) = do
    sym <- symE
    case sym of
      Symbol name _ -> defConst name
      _             -> throwError $ CanNotRedefine "" $ Just info
  where
    defConst name = do
      val <- exprE
      env <- State.get
      State.put $ Env.register name val env
      return . Unit $ Just info

evalAcc (AppF funcE argEs, info) = do
    val <- funcE
    args <- sequence argEs
    supplyInfo info $ apply val args

evalAcc (_, i) = throwError . InvalidForm $ Just i

apply :: Val i -> [Val i] -> Either (Error i) (Val i)
apply (Builtin func _) args = func args
apply v _ = throwError $ CanNotCallAsFunction (pretty v) Nothing

eval :: Expr i -> Env (Val i) -> EvalResult i
eval expr = runEval $ cata (evalAcc . unAnnF) expr
