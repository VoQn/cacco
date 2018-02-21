{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Cacco.Error
  ( module Cacco.Error.IsError
  , Error(..)
  , message
  , unknownSymbol
  , TypeMismatch
  , ArityMismatch
  , arityMismatch
  , typeMismatch
  , info
  , hasInfo
  , setInfo
  , supplyInfo
  ) where

import           Data.Maybe                (isJust)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)

import           Cacco.Error.ArityMismatch (ArityMismatch)
import qualified Cacco.Error.ArityMismatch as Arity
import           Cacco.Error.IsError
import           Cacco.Error.TypeMismatch  (TypeMismatch)
import           Cacco.Error.TypeMismatch  as TypeMismatch

data Error i
  = Message String (Maybe i)
  | UnknownSymbol String (Maybe i)
  | CanNotRedefine String (Maybe i)
  | CanNotCallAsFunction String (Maybe i)
  | InvalidForm (Maybe i)
  | ArityMismatch ArityMismatch (Maybe i)
  | TypeMismatch TypeMismatch (Maybe i)
  deriving (Eq, Show, Typeable, Generic)

message :: String -> Error i
message msg = Message msg Nothing

unknownSymbol :: String -> Error i
unknownSymbol sym = UnknownSymbol sym Nothing



info :: Error i -> Maybe i
info err = case err of
    Message              _ i -> i
    UnknownSymbol        _ i -> i
    CanNotRedefine       _ i -> i
    CanNotCallAsFunction _ i -> i
    InvalidForm            i -> i
    ArityMismatch        _ i -> i
    TypeMismatch         _ i -> i

hasInfo :: Error i -> Bool
hasInfo = isJust . info

setInfo :: Maybe i -> Error i -> Error i
setInfo i e = ($ i) $ case e of
    Message              x _ -> Message x
    UnknownSymbol        x _ -> UnknownSymbol x
    CanNotRedefine       x _ -> CanNotRedefine x
    CanNotCallAsFunction x _ -> CanNotCallAsFunction x
    InvalidForm            _ -> InvalidForm
    ArityMismatch        x _ -> ArityMismatch x
    TypeMismatch         x _ -> TypeMismatch x

supplyInfo :: i -> Error i -> Error i
supplyInfo i e
  | hasInfo e = e
  | otherwise = setInfo (Just i) e

arityMismatch :: Error i
arityMismatch = ArityMismatch Arity.defaultError Nothing

typeMismatch :: Error i
typeMismatch = TypeMismatch TypeMismatch.defaultError Nothing
