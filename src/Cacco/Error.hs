{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Cacco.Error
  ( module Cacco.Error.IsError
  , Error(..)
  , TypeMismatch
  , ArityMismatch
  , arityMismatch
  , typeMismatch
  , info
  , hasInfo
  , setInfo
  ) where

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
  | InvalidForm (Maybe i)
  | ArityMismatch ArityMismatch (Maybe i)
  | TypeMismatch TypeMismatch (Maybe i)
  deriving (Eq, Show, Typeable, Generic)

info :: Error i -> Maybe i
info err = case err of
  Message _ i       -> i
  UnknownSymbol _ i -> i
  InvalidForm i     -> i
  ArityMismatch _ i -> i
  TypeMismatch _ i  -> i

hasInfo :: Error i -> Bool
hasInfo err = case info err of
  Nothing -> False
  _       -> True

setInfo :: Maybe i -> Error i -> Error i
setInfo i e = ($ i) $ case e of
  Message x _       -> Message x
  UnknownSymbol x _ -> UnknownSymbol x
  InvalidForm _     -> InvalidForm
  ArityMismatch x _ -> ArityMismatch x
  TypeMismatch x _  -> TypeMismatch x

arityMismatch :: Error i
arityMismatch = ArityMismatch Arity.defaultError Nothing

typeMismatch :: Error i
typeMismatch = TypeMismatch TypeMismatch.defaultError Nothing
