{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Cacco.Error.ArityMismatch (
    ArityMismatch,
    defaultError,
    funcName,
    arity,
    isVariadic,
    applied,
)
where

import Control.Lens
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Cacco.Error.IsError

-- | An error type mismatch function arguments count
data ArityMismatch = ArityMismatch
    { _funcName :: String
    -- ^ The function name
    , _arity :: Int
    -- ^ Arity count of the function
    , _isVariadic :: Bool
    -- ^ Is the function variadic
    , _applied :: Int
    -- ^ Applied arguments count
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''ArityMismatch

defaultError :: ArityMismatch
defaultError =
    ArityMismatch
        { _funcName = ""
        , _arity = 0
        , _isVariadic = False
        , _applied = 0
        }

instance IsError ArityMismatch where
    printError ArityMismatch{..} =
        unwords
            [ "arity mismatch:"
            , name
            , "required"
            , arguments ++ "."
            , "but got"
            , actual ++ "."
            ]
      where
        atLeast
            | _isVariadic = ""
            | otherwise = "at least "
        args
            | _arity <= 1 = "argument"
            | otherwise = "arguments"
        name = '`' : _funcName ++ "`"
        arguments = unwords [atLeast ++ show _arity, args]
        actual = show _applied
