{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Cacco.Type where

--
import Control.DeepSeq
import Data.Data
import GHC.Generics

data Type
    = TyUnit
    | TyBool
    | TyInt8
    | TyInt16
    | TyInt32
    | TyInt64
    | TyInteger
    | TyUint8
    | TyUint16
    | TyUint32
    | TyUint64
    | TyNat
    | TyFloat16
    | TyFloat32
    | TyFloat64
    | TyDecimal
    | TyVar Int
    | TyFun Type Type
    deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Type

data TypeSchema = TypeSchema [Int] Type
