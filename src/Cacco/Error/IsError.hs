module Cacco.Error.IsError (
    IsError (..),
)
where

class IsError a where
    printError :: a -> String
