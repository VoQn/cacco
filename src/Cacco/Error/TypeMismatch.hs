{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cacco.Error.TypeMismatch
  ( TypeMismatch
  , funcName
  , expectedType
  , applied
  , defaultError
  ) where

import           Control.Lens
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)

import           Cacco.Error.IsError

-- | An error type mismatch types between the function and applied arguments
data TypeMismatch = TypeMismatch
  { -- | The function name
    _funcName     :: String,
    -- | Expected type
    _expectedType :: String,
    -- | Applied Value
    _applied      :: String
  } deriving (Eq, Show, Typeable, Generic)

makeLenses ''TypeMismatch

defaultError :: TypeMismatch
defaultError = TypeMismatch
  { _funcName = ""
  , _expectedType = ""
  , _applied = ""
  }

instance IsError TypeMismatch where
  printError TypeMismatch{..} =
    unwords [
        "type mismatch:", name,
        "expected:", _expectedType, "type.",
        "but got:", acutal ++ "."
      ]
    where
      name = '`' : _funcName ++ "`"
      acutal = '`' : _applied ++ "`"
