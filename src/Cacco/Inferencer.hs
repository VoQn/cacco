{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cacco.Inferencer where

import Control.Monad.ST (
    ST,
    runST,
 )

import Data.Functor.Const
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (
    STRef,
    newSTRef,
    readSTRef,
    writeSTRef,
 )

import Cacco.Syntax.AST
import Cacco.Syntax.Index (IndexProxy (..))
import Cacco.Syntax.Literal
import Cacco.Syntax.Location
import Cacco.Type
import Control.Comonad.Ix.IxCofree
import Data.Functor.Ix

type Env = Map String Type

type VarInfo = (Int, Map Int Type)

createVar :: STRef s VarInfo -> ST s Type
createVar ref = do
    (nextIndex, varMap) <- readSTRef ref
    writeSTRef ref (nextIndex + 1, varMap)
    return $ TyVar nextIndex

-- | reference from Type-Variable-Map
refer :: Type -> Map Int Type -> Type
refer (TyFun p r) table = TyFun p' r'
  where
    p' = refer p table
    r' = refer r table
refer t@(TyVar i) table = case Map.lookup i table of
    Just vt -> refer vt table
    Nothing -> t
refer t _ = t

-- | Type Unification
unify :: Type -> Type -> STRef s VarInfo -> ST s ()
unify (TyFun ps1 rt1) (TyFun ps2 rt2) ref = do
    unify ps1 ps2 ref
    unify rt1 rt2 ref
unify (TyVar i1) (TyVar i2) _ | i1 == i2 = return ()
unify (TyVar i1) t2 ref = unifyVar i1 t2 ref
unify t1 (TyVar i2) ref = unifyVar i2 t1 ref
unify t1 t2 ref
    | t1 == t2 = return ()
    | otherwise = do
        (_, table) <- readSTRef ref
        error $
            "cannot unify: "
                ++ show (refer t1 table)
                ++ " <=> "
                ++ show
                    (refer t2 table)

unifyVar :: Int -> Type -> STRef s VarInfo -> ST s ()
unifyVar index ty ref = do
    isOccur <- occur ty index ref
    if isOccur
        then error "occurs error"
        else do
            (nextIndex, table) <- readSTRef ref
            case Map.lookup index table of
                Just vt -> unify vt ty ref
                Nothing ->
                    let nextTable = Map.insert index ty table
                     in writeSTRef ref (nextIndex, nextTable)

occur :: Type -> Int -> STRef s VarInfo -> ST s Bool
occur (TyFun p e) n ref = (||) <$> occur p n ref <*> occur e n ref
occur (TyVar i) n ref
    | i == n = return True
    | otherwise = do
        (_, varMap) <- readSTRef ref
        case Map.lookup i varMap of
            Just vt -> occur vt n ref
            Nothing -> return False
occur _ _ _ = return False

inferLit :: Literal -> Type
inferLit lit = case lit of
    Bool _ -> TyBool
    Int8 _ -> TyInt8
    Int16 _ -> TyInt16
    Int32 _ -> TyInt32
    Int64 _ -> TyInt64
    Integer _ -> TyInteger
    Uint8 _ -> TyUint8
    Uint16 _ -> TyUint16
    Uint32 _ -> TyUint32
    Uint64 _ -> TyUint64
    Natural _ -> TyNat
    Float16 _ -> TyFloat16
    Float32 _ -> TyFloat32
    Float64 _ -> TyFloat64
    Flonum _ -> TyDecimal
    _ -> undefined

infer :: [(String, Type)] -> Ann Location AstF ~>. Type
infer env expr = runST $ do
    varInfoRef <- newSTRef (0, Map.empty)
    t <- doInfer (Map.fromList env) varInfoRef expr
    (_, varDict) <- readSTRef varInfoRef
    return $ refer t varDict

doInfer :: forall s. Env -> STRef s VarInfo -> Ann Location AstF ~>. ST s Type
doInfer env ref = icata' alg
  where
    alg :: AnnF Location AstF (Const (ST s Type)) ~>. ST s Type
    alg (_ :<< LitF _ l) = return $ inferLit l
    alg (_ :<< VarF ExprProxy n) = case Map.lookup n env of
        Just t -> return t
        Nothing -> error ("not found: " ++ n)
    alg (_ :<< VarF PattProxy n) = do
        vt <- createVar ref
        let _env = Map.insert n vt env
        rt <- createVar ref
        return $ TyFun vt rt
    alg (_ :<< AppF (Const func) args) = do
        funType <- func
        argTypes <- traverse getConst args
        let retType = createVar ref
        argType <- foldr ((<$>) . TyFun) retType argTypes
        unify funType argType ref
        return funType
    alg (_ :<< IfF (Const c) (Const t) (Const e)) = do
        cond <- c
        if cond /= TyBool
            then error $ "expected Boolean but " ++ show cond
            else do
                thenType <- t
                elseType <- e
                if thenType == elseType
                    then return thenType
                    else
                        error $
                            "Type Mismath::cannot unite different types. "
                                ++ "then-case: "
                                ++ show thenType
                                ++ "else-case: "
                                ++ show elseType
    alg _ = undefined
