module Cacco.Inferencer where

import           Prelude          hiding (lookup)

import           Control.Monad.ST (ST, runST)

import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Monoid      ((<>))
import           Data.STRef       (STRef, newSTRef, readSTRef, writeSTRef)

import           Cacco.Expr
import           Cacco.Type

type Env = Map String Type

type VarInfo = (Int, Map Int Type)

infer :: [(String, Type)] -> Expr -> Type
infer env expr = runST $ do
  varInfoRef <- newSTRef (0, Map.empty)
  t <- doInfer (Map.fromList env) varInfoRef expr
  (_, varDict) <- readSTRef varInfoRef
  return $ refer t varDict

doInfer :: Env -> STRef s VarInfo -> Expr -> ST s Type
doInfer _ _ (Boolean _ _) = return TyBool
doInfer _ _ (Integer _ _) = return TyInteger
doInfer _ _ (Decimal _ _) = return TyDecimal
doInfer env _ (Symbol _ n) =
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
