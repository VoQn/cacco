
module Cacco.Eval where

import           Cacco.Core     (Env, EvalF, add, eq, mul, sub)
import           Cacco.Expr     (Annotated, ExprF (..), Info (..))
import           Cacco.Fix      (fold)
import qualified Cacco.Literal  as Lit
import           Cacco.Location (Location)
import           Cacco.Val      (Val (..))
import           Control.Monad  (sequence)
import           Data.Function  ((&))
import           Data.Monoid    ((<>))

evalAcc :: Info Location ExprF EvalF -> EvalF
evalAcc (Info i (LitF l)) = lit l
  where
    lit (Lit.Boolean b) = const $ return $ Bool b i
    lit (Lit.Integer v) = const $ return $ Integer v i

evalAcc (Info i (SymF s)) = case s of
  "+"    -> const $ return $ Symbol "+" i
  "-"    -> const $ return $ Symbol "-" i
  "*"    -> const $ return $ Symbol "*" i
  "=="   -> const $ return $ Symbol "==" i
  "list" -> const $ return $ Symbol "list" i
  _      -> const $ Left $ "unknown symbol: `" <> s <> "` " <> show i

evalAcc (Info i (LisF exprs)) = \env ->
  case sequence $ (env &) <$> exprs of
    Left err -> Left err
    Right [] -> Right $ Unit i
    Right vals@(v:vs) -> case v of
      Symbol "+" _    -> add i vs
      Symbol "-" _    -> sub i vs
      Symbol "*" _    -> mul i vs
      Symbol "==" _   -> eq i vs
      Symbol "list" _ -> Right $ List vals i

evalAcc (Info i _) = const $ Left (show i)

eval :: Annotated Location -> Env -> Either String Val
eval = fold evalAcc
