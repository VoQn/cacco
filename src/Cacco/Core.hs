module Cacco.Core
  (
    Env, EvalF, BuiltInFunc
  , eq
  , add, sub, mul
  )
  where

import           Cacco.Error    (ArityMismatch (..), Error (..),
                                 TypeMismatch (..), printError)
import           Cacco.Location (Location)
import           Cacco.Val      (Val (..))

type Env = [(String, Val)]
type EvalF = Env -> Either String Val

type BuiltInFunc = Location -> [Val] -> Either String Val

raise :: Error e => e -> Either String a
raise = Left . printError

eqTypeError :: String -> Val -> TypeMismatch
eqTypeError = TypeMismatch "=="

eqArityError :: Location -> Int -> ArityMismatch
eqArityError l = ArityMismatch l "==" 2 False

eq :: BuiltInFunc
eq l [] = raise $ eqArityError l 0
eq l [_] = raise $ eqArityError l 1

eq l (Bool x _ : xs) = Bool <$> acc xs <*> pure l
  where
    acc :: [Val] -> Either String Bool
    acc [] = return True
    acc (Bool y _ : ys)
      | x /= y = return False
      | x == y = acc ys
    acc (a : _) = raise $ eqTypeError "boolean" a

eq l (Integer x _ : xs) = Bool <$> acc xs <*> pure l
  where
    acc :: [Val] -> Either String Bool
    acc [] = return True
    acc (Integer y _ : ys)
      | x /= y = return False
      | x == y = acc ys
    acc (a : _) = raise $ eqTypeError "integer" a

eq _ (a : _) = raise $ eqTypeError "boolean" a

add :: BuiltInFunc
add l [] = raise $ ArityMismatch l "+" 1 False 0
add l (Integer x _ : xs) = Integer <$> acc x xs <*> pure l
  where
    acc :: Integer -> [Val] -> Either String Integer
    acc r []                 = return r
    acc r (Integer y _ : ys) = acc (r + y) ys
    acc _ (a : _)            = raise $ TypeMismatch "+" "numeric" a
add _ (x : _) = raise $ TypeMismatch "+" "numeric" x

sub :: BuiltInFunc
-- arity mismatch
sub l [] = raise $ ArityMismatch l "-" 1 False 0
-- nenagte
sub l [Integer x _] = return $ Integer (-x) l
-- subtraction
sub l (Integer x _ : rest) = Integer <$> acc x rest <*> pure l
  where
    acc :: Integer -> [Val] -> Either String Integer
    acc r []                 = return r
    acc r (Integer y _ : ys) = acc (r - y) ys
    acc _ (a : _)            = raise $ TypeMismatch "-" "numeric" a
-- other case is mismatch types
sub _ (x : _) = raise $ TypeMismatch "-" "numeric" x

mul :: BuiltInFunc
mul l [] = raise $ ArityMismatch l "*" 1 False 0
mul l (Integer x _ : vs) = Integer <$> acc x vs <*> pure l
  where
    acc :: Integer -> [Val] -> Either String Integer
    acc r []                 = return r
    acc r (Integer y _ : ys) = acc (r * y) ys
    acc _ (a : _)            = raise $ TypeMismatch "*" "numeric" a
mul _ (a : _) = raise $ TypeMismatch "*" "numeric" a
