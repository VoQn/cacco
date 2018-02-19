
module Cacco.Eval where

import           Control.Monad         (sequence)

import           Cacco.Ann             (AnnF (..))
import           Cacco.Core            (Env, EvalF)
import qualified Cacco.Core            as Core
import           Cacco.Fix             (cata)
import           Cacco.Syntax.Expr     (AstF (..), Expr)
import qualified Cacco.Syntax.Literal  as Lit
import           Cacco.Syntax.Location (Location)
import           Cacco.Val             (Val (..))

evalAcc :: (Location, AstF EvalF) -> EvalF
evalAcc (i, LitF l) = const $ case l of
  Lit.Undef     -> Left $ unwords ["undefined", show i]
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
