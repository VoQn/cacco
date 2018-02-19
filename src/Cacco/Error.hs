{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Cacco.Error where

import           Control.Lens
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic)

import           Cacco.Syntax.Location (Location)
import           Cacco.Val             (Val, info, pretty)

class Error a where
  printError :: a -> String

-- | An error type mismatch function arguments count
data ArityMismatch = ArityMismatch
  { -- | Location at the function was called
    _location   :: Location,
    -- | The function name
    _funcName   :: String,
    -- | Arity count of the function
    _arity      :: Int,
    -- | Is the function variadic
    _isVariadic :: Bool,
    -- | Applied arguments count
    _applied    :: Int
  } deriving (Eq, Ord, Show, Typeable, Generic)

makeFields ''ArityMismatch

instance Error ArityMismatch where
  printError ArityMismatch{..} =
      unwords [
        "arity mismatch:", name,
        "required", arguments ++ ".",
        "but got", actual ++ ".",
        location
      ]
    where
      atLeast
        | _isVariadic = ""
        | otherwise = "at least "
      args
        | _arity <= 1 = "argument"
        | otherwise = "arguments"
      name = '`' : _funcName ++ "`"
      arguments = unwords [ atLeast ++ show _arity, args ]
      actual = show _applied
      location = show _location

-- | An error type mismatch types between the function and applied arguments
data TypeMismatch = TypeMismatch
  { -- | The function name
    _funcName     :: String,
    -- | Expected type
    _expectedType :: String,
    -- | Applied Value
    _applied      :: Val Location
  } deriving (Eq, Show, Typeable, Generic)

makeFields ''TypeMismatch

instance Error TypeMismatch where
  printError TypeMismatch{..} =
    unwords [
        "type mismatch:", name,
        "expected:", _expectedType, "type.",
        "but got:", acutal ++ ".",
        location
      ]
    where
      name = '`' : _funcName ++ "`"
      acutal = '`' : pretty _applied ++ "`"
      location = show (info _applied)
