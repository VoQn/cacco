{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Cacco.Val where

import           Cacco.Location (Location)
import           Data.Monoid    ((<>))
import           Data.Typeable  (Typeable)
import           GHC.Generics   (Generic)

data Val
  = Unit Location
  | Bool !Bool Location
  | Integer !Integer Location
  | Symbol !String Location
  | List [Val] Location
  deriving (Eq, Ord, Show, Typeable, Generic)

getLocation :: Val -> Location
getLocation v = case v of
  Unit l      -> l
  Bool _ l    -> l
  Integer _ l -> l
  Symbol _ l  -> l
  List _ l    -> l

pretty :: Val -> String
pretty val = case val of
  Unit _ -> "()"
  Bool True _  -> "true"
  Bool False _ -> "false"
  Integer x _  -> show x
  Symbol s _   -> s
  List [] _ -> "()"
  List (v : vs) _ -> case v of
    Symbol "list" _ -> "`(" <> unwords (pretty <$> vs) <> ")"
    _               -> "(" <> unwords (pretty <$> (v:vs)) <> ")"
