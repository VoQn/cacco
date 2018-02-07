module Cacco.Inferencer where

import           Prelude          hiding (lookup)

import           Control.Monad.ST (ST, runST)

import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Monoid      ((<>))
import           Data.STRef       (STRef, newSTRef, readSTRef)

import           Cacco.Ast
import qualified Cacco.Literal    as Literal
import           Cacco.Location
import           Cacco.Type

type Env = Map String Type

type VarInfo = (Int, Map Int Type)

infer :: [(String, Type)] -> Ast Location -> Type
infer env expr = runST $ do
  varInfoRef <- newSTRef (0, Map.empty)
  t <- doInfer (Map.fromList env) varInfoRef expr
  (_, varDict) <- readSTRef varInfoRef
  return $ refer t varDict

doInfer :: Env -> STRef s VarInfo -> Ast Location -> ST s Type
doInfer _ _ (Literal x) = case x of
  (Literal.Boolean _) -> return TyBool
  (Literal.Integer _) -> return TyInteger
  (Literal.Decimal _) -> return TyDecimal

doInfer env ref (With _ l@(Literal _)) = doInfer env ref l

doInfer env _ (With _ (Symbol n)) =
  case Map.lookup n env of
    Just t  -> return t
    Nothing -> error ("not found: " <> n)

doInfer _ _ _ = undefined

refer :: Type -> Map Int Type -> Type
refer (TyFunc p r) varMap = TyFunc ((`refer` varMap) <$> p) $ refer r varMap

refer t@(TyVar i) varMap = case Map.lookup i varMap of
  Just vt -> refer vt varMap
  Nothing -> t

refer t _ = t
