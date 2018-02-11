{-# LANGUAGE BangPatterns #-}

module Cacco.Core where

import           Cacco.Location (Location)
import           Data.Monoid    ((<>))

data Val
  = ValUnit Location
  | ValBool !Bool Location
  | ValInteger !Integer Location
  | ValSymbol !String Location
  | ValList [Val] Location
  deriving (Eq, Ord, Show)

getLocation :: Val -> Location
getLocation v = case v of
  ValUnit l      -> l
  ValBool _ l    -> l
  ValInteger _ l -> l
  ValSymbol _ l  -> l
  ValList _ l    -> l

pretty :: Val -> String
pretty val = case val of
  ValUnit _ -> "()"
  ValBool True _  -> "true"
  ValBool False _ -> "false"
  ValInteger x _  -> show x
  ValSymbol s _   -> s
  ValList [] _ -> "()"
  ValList (v : vs) _ -> case v of
    ValSymbol "list" _ -> "(" <> unwords (pretty <$> vs) <> ")"
    _                  -> "(" <> unwords (pretty <$> (v:vs)) <> ")"

type Env = [(String, Val)]
type EvalF = Env -> Either String Val

type BuiltInFunc = Location -> [Val] -> Either String Val

arityMismatch :: Location -- ^ Location at the function was called
              -> String   -- ^ Function name
              -> Int      -- ^ Require count of arguments
              -> Bool     -- ^ is fixed number arguments or not
              -> Int      -- ^ Actural applied arguments count
              -> String
arityMismatch location name required isFixed applied =
  let
    atLeast
      | isFixed = ""
      | otherwise = "at least "
    arguments
      | required <= 1 = "argument"
      | otherwise = "arguments"
  in
    "arity mismatch: " <> "`" <> name <> "` required " <>
    atLeast <> show required <> " " <> arguments <> "." <>
    "but got " <> show applied <> ". " <>
    show location

typeMismatch :: String -- ^ Function name
             -> String -- ^ Expected type
             -> Val    -- ^ Argument value
             -> String
typeMismatch name expected value =
  "type mismatch: "<> "`" <> name <> "` " <>
  "expected " <> expected <> " type. " <>
  "but got `" <> pretty value <> "`. " <>
  show (getLocation value)

eq :: BuiltInFunc
eq l [] = Left $ arityMismatch l "==" 2 False 0
eq l [_] = Left $ arityMismatch l "==" 2 False 1

eq l (ValBool x _ : xs) = ValBool <$> acc xs <*> pure l
  where
    acc :: [Val] -> Either String Bool
    acc [] = return True
    acc (ValBool y _ : ys)
      | x /= y = return False
      | x == y = acc ys
    acc (a : _) = Left $ typeMismatch "==" "boolean" a

eq l (ValInteger x _ : xs) = ValBool <$> acc xs <*> pure l
  where
    acc :: [Val] -> Either String Bool
    acc [] = return True
    acc (ValInteger y _ : ys)
      | x /= y = return False
      | x == y = acc ys
    acc (a : _) = Left $ typeMismatch "==" "integer" a

eq _ (a : _) = Left $ typeMismatch "==" "boolean" a

add :: BuiltInFunc
add l [] = Left $ arityMismatch l "+" 1 False 0
add l (ValInteger x _ : xs) = ValInteger <$> acc x xs <*> pure l
  where
    acc :: Integer -> [Val] -> Either String Integer
    acc !r vs = case vs of
      []                    -> return r
      (ValInteger y _ : ys) -> acc (r + y) ys
      (a:_)                 -> Left $ typeMismatch "+" "numeric" a
add _ (x : _) = Left $ typeMismatch "+" "numeric" x

sub :: BuiltInFunc
-- arity mismatch
sub l [] = Left $ arityMismatch l "-" 1 False 0
-- nenagte
sub l [ValInteger x _] = return $ ValInteger (-x) l
-- subtraction
sub l (ValInteger x _ : rest) = ValInteger <$> acc x rest <*> pure l
  where
    acc :: Integer -> [Val] -> Either String Integer
    acc r []                    = return r
    acc r (ValInteger y _ : ys) = acc (r - y) ys
    acc _ (a : _)               = Left $ typeMismatch "-" "numeric" a
-- other case is mismatch types
sub _ (x : _) = Left $ typeMismatch "-" "numeric" x

mul :: BuiltInFunc
mul l [] = Left $ arityMismatch l "*" 1 False 0
mul l (ValInteger x _ : vs) = ValInteger <$> acc x vs <*> pure l
  where
    acc :: Integer -> [Val] -> Either String Integer
    acc r []                    = return r
    acc r (ValInteger y _ : ys) = acc (r * y) ys
    acc _ (a : _)               = Left $ typeMismatch "*" "numeric" a
mul _ (a : _) = Left $ typeMismatch "*" "numeric" a
