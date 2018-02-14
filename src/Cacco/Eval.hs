
module Cacco.Eval where

import           Control.Monad  (sequence)
import           Data.Function  ((&))
import           Data.Monoid    ((<>))

import           Cacco.Ann      (AnnF (..))
import           Cacco.Core     (Env, EvalF, add, eq, mul, sub)
import           Cacco.Expr     (AstF (..), Expr)
import           Cacco.Fix      (cata)
import qualified Cacco.Literal  as Lit
import           Cacco.Location (Location)
import           Cacco.Val      (Val (..))

evalAcc :: (Location, AstF EvalF) -> EvalF
evalAcc (i, LitF x) = evalLit x
  where
    evalLit (Lit.Bool b)    = const $ return $ Bool b i
    evalLit (Lit.Integer v) = const $ return $ Integer v i

evalAcc (i, SymF s) = case s of
  "+"    -> const $ return $ Symbol "+" i
  "-"    -> const $ return $ Symbol "-" i
  "*"    -> const $ return $ Symbol "*" i
  "=="   -> const $ return $ Symbol "==" i
  "list" -> const $ return $ Symbol "list" i
  _      -> const $ Left $ "unknown symbol: `" <> s <> "` " <> show i

evalAcc (i, LisF exprs) = \env ->
  case sequence $ (env &) <$> exprs of
    Left err -> Left err
    Right [] -> Right $ Unit i
    Right vals@(v:vs) -> case v of
      Symbol "+" _    -> add i vs
      Symbol "-" _    -> sub i vs
      Symbol "*" _    -> mul i vs
      Symbol "==" _   -> eq i vs
      Symbol "list" _ -> Right $ List vals i

evalAcc (i, _) = const $ Left (show i)

eval :: Expr Location -> Env -> Either String Val
eval = cata (evalAcc . unAnnF)
