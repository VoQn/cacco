
module Cacco.Eval where

import           Cacco.Core     (Env, EvalF, Val (..), add, eq, mul, sub)
import           Cacco.Expr     (AnnExpr, ExprF (..), Info (..))
import qualified Cacco.Expr     as Expr
import           Cacco.Fix      (fold)
import qualified Cacco.Literal  as Lit
import           Cacco.Location (Location)
import           Control.Monad  (sequence)
import           Data.Function  ((&))
import           Data.Monoid    ((<>))

evalAcc :: Info Location ExprF EvalF -> EvalF
evalAcc (Info i (LitF l)) = lit l
  where
    lit (Lit.Boolean b) = const $ return $ ValBool b i
    lit (Lit.Integer v) = const $ return $ ValInteger v i

evalAcc (Info i (SymF s)) = case s of
  "+"    -> const $ return $ ValSymbol "+" i
  "-"    -> const $ return $ ValSymbol "-" i
  "*"    -> const $ return $ ValSymbol "*" i
  "=="   -> const $ return $ ValSymbol "==" i
  "list" -> const $ return $ ValSymbol "list" i
  _      -> const $ Left $ "unknown symbol: `" <> s <> "` " <> show i

evalAcc (Info i (LisF exprs)) = \env ->
  case sequence $ (env &) <$> exprs of
    Left err -> Left err
    Right [] -> Right $ ValUnit i
    Right vals@(v:vs) -> case v of
      ValSymbol "+" _    -> add i vs
      ValSymbol "-" _    -> sub i vs
      ValSymbol "*" _    -> mul i vs
      ValSymbol "==" _   -> eq i vs
      ValSymbol "list" _ -> Right $ ValList vals i

evalAcc (Info i _) = const $ Left (show i)

eval :: AnnExpr Location -> Env -> Either String Val
eval = fold evalAcc
