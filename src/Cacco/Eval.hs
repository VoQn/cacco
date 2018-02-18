
module Cacco.Eval where

import           Control.Monad  (sequence)

import           Cacco.Ann      (AnnF (..))
import           Cacco.Core     (Env, EvalF)
import qualified Cacco.Core     as Core
import           Cacco.Expr     (AstF (..), Expr)
import           Cacco.Fix      (cata)
import qualified Cacco.Literal  as Lit
import           Cacco.Location (Location)
import           Cacco.Val      (Val (..))

evalAcc :: (Location, AstF EvalF) -> EvalF
evalAcc (i, LitF l) = case l of
  Lit.Undef     -> const $ Left $ unwords ["undefined", show i]
  Lit.Bool    x -> const $ return $ Bool x $ Just i
  Lit.Int8    x -> const $ return $ Int8 x $ Just i
  Lit.Int16   x -> const $ return $ Int16 x $ Just i
  Lit.Int32   x -> const $ return $ Int32 x $ Just i
  Lit.Int64   x -> const $ return $ Int64 x $ Just i
  Lit.Uint8   x -> const $ return $ Uint8 x $ Just i
  Lit.Uint16  x -> const $ return $ Uint16 x $ Just i
  Lit.Uint32  x -> const $ return $ Uint32 x $ Just i
  Lit.Uint64  x -> const $ return $ Uint64 x $ Just i
  Lit.Integer x -> const $ return $ Integer x $ Just i

evalAcc (i, SymF s) = case s of
  "+"    -> const $ return $ Symbol "+" $ Just i
  "-"    -> const $ return $ Symbol "-" $ Just i
  "*"    -> const $ return $ Symbol "*" $ Just i
  "=="   -> const $ return $ Symbol "==" $ Just i
  "!="   -> const $ return $ Symbol "!=" $ Just i
  ">"    -> const $ return $ Symbol ">" $ Just i
  ">="   -> const $ return $ Symbol ">=" $ Just i
  "<"    -> const $ return $ Symbol "<" $ Just i
  "<="   -> const $ return $ Symbol "<=" $ Just i
  "list" -> const $ return $ Symbol "list" $ Just i
  _      -> const $ Left $ unwords ["unknown symbol:", '`' : s ++ "`", show i]

evalAcc (i, LisF exprs) = \env ->
  case sequence $ ($ env) <$> exprs of
    Left err -> Left err
    Right [] -> Right $ Unit $ Just i
    Right vals@(v:vs) -> case v of
      Symbol "+" _    -> Core.add i vs
      Symbol "-" _    -> Core.sub i vs
      Symbol "*" _    -> Core.mul i vs
      Symbol "==" _   -> Core.eq i vs
      Symbol "!=" _   -> Core.neq i vs
      Symbol ">" _    -> Core.gt i vs
      Symbol ">=" _   -> Core.gte i vs
      Symbol "<" _    -> Core.lt i vs
      Symbol "<=" _   -> Core.lte i vs
      Symbol "list" _ -> Right $ List vals $ Just i

evalAcc (i, _) = const $ Left (show i)

eval :: Expr Location -> Env -> Either String (Val Location)
eval = cata (evalAcc . unAnnF)
