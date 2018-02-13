{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Cacco.Error where
import           Cacco.Val      (Val, getLocation, pretty)
import           Data.Typeable  (Typeable)
import           GHC.Generics   (Generic)

import           Cacco.Location (Location)
import           Control.Lens

class Error a where
  printError :: a -> String

-- | An error type mismatch function arguments count
data ArityMismatch = ArityMismatch
  {
    _location   :: Location, -- ^ Location at the function was called
    _funcName   :: String,   -- ^ The function name
    _arity      :: Int,      -- ^ Arity count of the function
    _isVariadic :: Bool,     -- ^ Is the function variadic
    _applied    :: Int       -- ^ Applied arguments count
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
  {
    _funcName     :: String, -- ^ The function name
    _expectedType :: String, -- ^ Expected type
    _applied      :: Val     -- ^ Applied Value
  } deriving (Eq, Ord, Show, Typeable, Generic)

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
      location = show (getLocation _applied)
