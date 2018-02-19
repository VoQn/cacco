
module Cacco.Eval where

import           Control.Monad         (sequence)
-- import           Data.Map              (Map)
import qualified Data.Map              as Map

import           Cacco.Ann             (AnnF (..))
import           Cacco.Core            (Env, EvalF)
-- import qualified Cacco.Core            as Core
import           Cacco.Error           (Error (..))
import qualified Cacco.Error           as Error
import           Cacco.Fix             (cata)
import           Cacco.Syntax.Expr     (AstF (..), Expr)
import qualified Cacco.Syntax.Literal  as Lit
import           Cacco.Syntax.Location (Location)
import           Cacco.Val             (Val (..))
import qualified Cacco.Val             as Val

evalAcc :: (Location, AstF (EvalF Location)) -> EvalF Location
evalAcc (i, LitF l) = const $ case l of
  Lit.Undef     -> Left $ Message "undefined" (Just i)
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

evalAcc (i, SymF s) = \env -> case Map.lookup s env of
  Just sym -> Right sym
  Nothing  -> Left $ UnknownSymbol s $ Just i

evalAcc (i, AppF fn args) = \env -> do
    res <- fn env
    case res of
      Builtin f _ -> do
        vs <- sequence $ ($ env) <$> args
        setLocation $ f vs
  where
    setLocation result = case result of
      err@(Left e)
        | Error.hasInfo e -> err
        | otherwise -> Left $ Error.setInfo (Just i) e
      Right _ -> Val.setInfo (Just i) <$> result

evalAcc (i, _) = const $ Left $ InvalidForm $ Just i

eval :: Expr Location -> Env Location -> Either (Error Location) (Val Location)
eval = cata (evalAcc . unAnnF)
