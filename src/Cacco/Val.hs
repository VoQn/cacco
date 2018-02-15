{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cacco.Val
  ( Val(..)
  , Fn
  , info
  , pretty
  ) where

import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vec
import           GHC.Generics    (Generic)

data Val ann
  = Unit ann

  | Bool    !Bool ann
  | Integer !Integer ann
  | Flonum  !Scientific ann
  | Text    !Text ann

  | Symbol  !String ann

  | List    [Val ann] ann
  | Vector  (Vector (Val ann)) ann
  | Struct  (Map String (Val ann)) ann

  | BultIn  (Fn ann) ann
  | Func    (Fn ann) ann
  deriving (Typeable, Generic)

type Fn ann = [Val ann] -> Either String (Val ann)

instance Eq (Val a) where
  (==) (Unit _) (Unit _)           = True
  (==) (Bool x _) (Bool y _)       = x == y
  (==) (Integer x _) (Integer y _) = x == y
  (==) (Flonum x _) (Flonum y _)   = x == y
  (==) (Symbol x _) (Symbol y _)   = x == y
  (==) (Text x _) (Text y _)       = x == y
  (==) (List x _) (List y _)       = x == y
  (==) (Vector x _) (Vector y _)   = x == y
  (==) (Struct x _) (Struct y _)   = x == y
  (==) _ _                         = False

instance (Show a) => Show (Val a) where
  show (Unit i)      = unwords ["Unit", show i]
  show (Bool x i)    = unwords ["Bool", show x, show i]
  show (Integer x i) = unwords ["Integer", show x, show i]
  show (Flonum x i)  = unwords ["Flonum", show x, show i]
  show (Text x i)    = unwords ["Text", show x, show i]
  show (Symbol x i)  = unwords ["Symbol", show x, show i]
  show (List xs i)   = unwords ["List", show xs, show i]
  show (Vector xs i) = unwords ["Vector", show xs, show i]
  show (Struct x i)  = unwords ["Struct", show x, show i]
  show (BultIn _ i)  = unwords ["BultIn", show i]
  show (Func _ i)    = unwords ["Func", show i]

info :: Val i -> i
info (Unit i)      = i
info (Bool _ i)    = i
info (Integer _ i) = i
info (Flonum _ i)  = i
info (Text _ i)    = i
info (Symbol _ i)  = i
info (List _ i)    = i
info (Vector _ i)  = i
info (Struct _ i)  = i
info (BultIn _ i)  = i
info (Func _ i)    = i

pretty :: Val i -> String
pretty (Unit _)       = "()"
pretty (Bool True _)  = "true"
pretty (Bool False _) = "false"
pretty (Integer x _)  = show x
pretty (Flonum x _)   = show x
pretty (Text x _)     = show x
pretty (Symbol x _)   = x
pretty (List xs _)    = '(' : unwords (pretty <$> xs) ++ ")"
pretty (Vector xs _)  = '[' : unwords (pretty <$> Vec.toList xs) ++ "]"
pretty (Struct x _)   = '{' : unwords (prettyPair <$> Map.toList x) ++ "}"
pretty (BultIn _ _)   = "<#Bult_In#>"
pretty (Func _ _)     = "<#Function#>"

prettyPair :: (String, Val i) -> String
prettyPair (k, v) = k ++ ": " ++ pretty v
{-# INLINE prettyPair #-}
