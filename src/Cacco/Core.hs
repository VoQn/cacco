module Cacco.Core
  ( Env, EvalF, BuiltInFunc
  , eq, neq
  , gt, gte, lt, lte
  , add, sub, mul
  )
  where

--
import           Data.Int
import           Data.Word

import           Cacco.Error    (ArityMismatch (..), Error (..),
                                 TypeMismatch (..), printError)
import           Cacco.Location (Location)
import           Cacco.Val      (Val (..))

type Env = [(String, Val Location)]
type EvalF = Env -> Either String (Val Location)

type BuiltInFunc = Location -> [Val Location] -> Either String (Val Location)

raise :: Error e => e -> Either String a
raise = Left . printError
--
untilBool :: String -> (Bool -> Bool -> Bool) -> Bool -> [Val Location] -> Either String Bool
untilBool _ _ _ [] = return True
untilBool n f x (v:r) = case v of
  Bool y _ -> if f x y then return False else untilBool n f y r
  _        -> raise $ TypeMismatch n "Bool" v
--
untilI8 :: String -> (Int8 -> Int8 -> Bool) -> Int8 -> [Val Location] -> Either String Bool
untilI8 _ _ _ [] = return True
untilI8 n f x (v:r) = case v of
  Int8 y _ -> if f x y then return False else untilI8 n f y r
  _        -> raise $ TypeMismatch n "Int8" v
--
untilI16 :: String -> (Int16 -> Int16 -> Bool) -> Int16 -> [Val Location] -> Either String Bool
untilI16 _ _ _ [] = return True
untilI16 n f x (v:r) = case v of
  Int16 y _ -> if f x y then return False else untilI16 n f y r
  _         -> raise $ TypeMismatch n "Int16" v
--
untilI32 :: String -> (Int32 -> Int32 -> Bool) -> Int32 -> [Val Location] -> Either String Bool
untilI32 _ _ _ [] = return True
untilI32 n f x (v:r) = case v of
  Int32 y _ -> if f x y then return False else untilI32 n f y r
  _         -> raise $ TypeMismatch n "Int32" v
--
untilI64 :: String -> (Int64 -> Int64 -> Bool) -> Int64 -> [Val Location] -> Either String Bool
untilI64 _ _ _ [] = return True
untilI64 n f x (v:r) = case v of
  Int64 y _ -> if f x y then return False else untilI64 n f y r
  _         -> raise $ TypeMismatch n "Int64" v
--
untilU8 :: String -> (Word8 -> Word8 -> Bool) -> Word8 -> [Val Location] -> Either String Bool
untilU8 _ _ _ [] = return True
untilU8 n f x (v:r) = case v of
  Uint8 y _ -> if f x y then return False else untilU8 n f y r
  _         -> raise $ TypeMismatch n "Uint8" v
--
untilU16 :: String -> (Word16 -> Word16 -> Bool) -> Word16 -> [Val Location] -> Either String Bool
untilU16 _ _ _ [] = return True
untilU16 n f x (v:r) = case v of
  Uint16 y _ -> if f x y then return False else untilU16 n f y r
  _          -> raise $ TypeMismatch n "Uint16" v
--
untilU32 :: String -> (Word32 -> Word32 -> Bool) -> Word32 -> [Val Location] -> Either String Bool
untilU32 _ _ _ [] = return True
untilU32 n f x (v:r) = case v of
  Uint32 y _ -> if f x y then return False else untilU32 n f y r
  _          -> raise $ TypeMismatch n "Uint32" v
--
untilU64 :: String -> (Word64 -> Word64 -> Bool) -> Word64 -> [Val Location] -> Either String Bool
untilU64 _ _ _ [] = return True
untilU64 n f x (v:r) = case v of
  Uint64 y _ -> if f x y then return False else untilU64 n f y r
  _          -> raise $ TypeMismatch n "Uint64" v
--
untilInteger :: String -> (Integer -> Integer -> Bool) -> Integer -> [Val Location] -> Either String Bool
untilInteger _ _ _ [] = return True
untilInteger n f x (v:r) = case v of
  Integer y _ -> if f x y then return False else untilInteger n f y r
  _           -> raise $ TypeMismatch n "Integer" v

eqArityError :: Location -> Int -> ArityMismatch
eqArityError l = ArityMismatch l "==" 2 False

eq :: BuiltInFunc
eq l [] = raise $ eqArityError l 0
eq l [_] = raise $ eqArityError l 1
eq l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
  Bool    x _ -> untilBool "==" (/=) x vs
  Int8    x _ -> untilI8   "==" (/=) x vs
  Int16   x _ -> untilI16  "==" (/=) x vs
  Int32   x _ -> untilI32  "==" (/=) x vs
  Int64   x _ -> untilI64  "==" (/=) x vs
  Uint8   x _ -> untilU8   "==" (/=) x vs
  Uint16  x _ -> untilU16  "==" (/=) x vs
  Uint32  x _ -> untilU32  "==" (/=) x vs
  Uint64  x _ -> untilU64  "==" (/=) x vs
  Integer x _ -> untilInteger "==" (/=) x vs
  _           -> raise $ TypeMismatch "==" "Eq" v

neq :: BuiltInFunc
neq l [] = raise $ ArityMismatch l "!=" 2 False 0
neq l [_] = raise $ ArityMismatch l "==" 2 False 1
neq l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
  Bool    x _ -> untilBool "!=" (==) x vs
  Int8    x _ -> untilI8   "!=" (==) x vs
  Int16   x _ -> untilI16  "!=" (==) x vs
  Int32   x _ -> untilI32  "!=" (==) x vs
  Int64   x _ -> untilI64  "!=" (==) x vs
  Uint8   x _ -> untilU8   "!=" (==) x vs
  Uint16  x _ -> untilU16  "!=" (==) x vs
  Uint32  x _ -> untilU32  "!=" (==) x vs
  Uint64  x _ -> untilU64  "!=" (==) x vs
  Integer x _ -> untilInteger "!=" (==) x vs
  _           -> raise $ TypeMismatch "!=" "Eq" v

gt :: BuiltInFunc
gt l [] = raise $ ArityMismatch l ">" 2 False 0
gt l [_] = raise $ ArityMismatch l ">" 2 False 1
gt l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
  Int8    x _ -> untilI8   ">" (<=) x vs
  Int16   x _ -> untilI16  ">" (<=) x vs
  Int32   x _ -> untilI32  ">" (<=) x vs
  Int64   x _ -> untilI64  ">" (<=) x vs
  Uint8   x _ -> untilU8   ">" (<=) x vs
  Uint16  x _ -> untilU16  ">" (<=) x vs
  Uint32  x _ -> untilU32  ">" (<=) x vs
  Uint64  x _ -> untilU64  ">" (<=) x vs
  Integer x _ -> untilInteger ">" (<=) x vs
  _           -> raise $ TypeMismatch ">" "Ord" v
--
gte :: BuiltInFunc
gte l [] = raise $ ArityMismatch l ">=" 2 False 0
gte l [_] = raise $ ArityMismatch l ">=" 2 False 0
gte l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
  Int8    x _ -> untilI8   ">=" (<) x vs
  Int16   x _ -> untilI16  ">=" (<) x vs
  Int32   x _ -> untilI32  ">=" (<) x vs
  Int64   x _ -> untilI64  ">=" (<) x vs
  Uint8   x _ -> untilU8   ">=" (<) x vs
  Uint16  x _ -> untilU16  ">=" (<) x vs
  Uint32  x _ -> untilU32  ">=" (<) x vs
  Uint64  x _ -> untilU64  ">=" (<) x vs
  Integer x _ -> untilInteger ">=" (<) x vs
  _           -> raise $ TypeMismatch ">=" "Ord" v
--
lt :: BuiltInFunc
lt l [] = raise $ ArityMismatch l "<" 2 False 0
lt l [_] = raise $ ArityMismatch l "<" 2 False 1
lt l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
  Int8    x _ -> untilI8   "<" (>=) x vs
  Int16   x _ -> untilI16  "<" (>=) x vs
  Int32   x _ -> untilI32  "<" (>=) x vs
  Int64   x _ -> untilI64  "<" (>=) x vs
  Uint8   x _ -> untilU8   "<" (>=) x vs
  Uint16  x _ -> untilU16  "<" (>=) x vs
  Uint32  x _ -> untilU32  "<" (>=) x vs
  Uint64  x _ -> untilU64  "<" (>=) x vs
  Integer x _ -> untilInteger "<" (>=) x vs
  _           -> raise $ TypeMismatch "<" "Ord" v
--
lte :: BuiltInFunc
lte l [] = raise $ ArityMismatch l "<=" 2 False 0
lte l [_] = raise $ ArityMismatch l "<=" 2 False 1
lte l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
  Int8    x _ -> untilI8   "<=" (>) x vs
  Int16   x _ -> untilI16  "<=" (>) x vs
  Int32   x _ -> untilI32  "<=" (>) x vs
  Int64   x _ -> untilI64  "<=" (>) x vs
  Uint8   x _ -> untilU8   "<=" (>) x vs
  Uint16  x _ -> untilU16  "<=" (>) x vs
  Uint32  x _ -> untilU32  "<=" (>) x vs
  Uint64  x _ -> untilU64  "<=" (>) x vs
  Integer x _ -> untilInteger "<=" (>) x vs
  _           -> raise $ TypeMismatch "<=" "Ord" v
--
accI8 :: String -> (a -> Int8 -> a) -> a -> [Val Location] -> Either String a
accI8 _ _ x []    = return x
accI8 n f x (v:r) = case v of
  Int8 y _ -> accI8 n f (f x y) r
  _        -> raise $ TypeMismatch n "Int8" v
--
accI16 :: String -> (a -> Int16 -> a) -> a -> [Val Location] -> Either String a
accI16 _ _ x []    = return x
accI16 n f x (v:r) = case v of
  Int16 y _ -> accI16 n f (f x y) r
  _         -> raise $ TypeMismatch n "Int16" v
--
accI32 :: String -> (a -> Int32 -> a) -> a -> [Val Location] -> Either String a
accI32 _ _ x []    = return x
accI32 n f x (v:r) = case v of
  Int32 y _ -> accI32 n f (f x y) r
  _         -> raise $ TypeMismatch n "Int32" v
--
accI64 :: String -> (a -> Int64 -> a) -> a -> [Val Location] -> Either String a
accI64 _ _ x []    = return x
accI64 n f x (v:r) = case v of
  Int64 y _ -> accI64 n f (f x y) r
  _         -> raise $ TypeMismatch n "Int64" v
--
accU8 :: String -> (a -> Word8 -> a) -> a -> [Val Location] -> Either String a
accU8 _ _ x []    = return x
accU8 n f x (v:r) = case v of
  Uint8 y _ -> accU8 n f (f x y) r
  _         -> raise $ TypeMismatch n "Uint8" v

accU16 :: String -> (a -> Word16 -> a) -> a -> [Val Location] -> Either String a
accU16 _ _ x []    = return x
accU16 n f x (v:r) = case v of
  Uint16 y _ -> accU16 n f (f x y) r
  _          -> raise $ TypeMismatch n "Uint16" v

accU32 :: String -> (a -> Word32 -> a) -> a -> [Val Location] -> Either String a
accU32 _ _ x []    = return x
accU32 n f x (v:r) = case v of
  Uint32 y _ -> accU32 n f (f x y) r
  _          -> raise $ TypeMismatch n "Uint32" v

accU64 :: String -> (a -> Word64 -> a) -> a -> [Val Location] -> Either String a
accU64 _ _ x []    = return x
accU64 n f x (v:r) = case v of
  Uint64 y _ -> accU64 n f (f x y) r
  _          -> raise $ TypeMismatch n "Uint64" v

accInteger :: String -> (a -> Integer -> a) -> a -> [Val Location] -> Either String a
accInteger _ _ x []    = return x
accInteger n f x (v:r) = case v of
  Integer y _ -> accInteger n f (f x y) r
  _           -> raise $ TypeMismatch n "Integer" v

add :: BuiltInFunc
add l []     = raise $ ArityMismatch l "+" 1 False 0
add l (v:r) = case v of
  Int8    x _ -> Int8    <$> accI8  "+" (+) x r <*> pure (Just l)
  Int16   x _ -> Int16   <$> accI16 "+" (+) x r <*> pure (Just l)
  Int32   x _ -> Int32   <$> accI32 "+" (+) x r <*> pure (Just l)
  Int64   x _ -> Int64   <$> accI64 "+" (+) x r <*> pure (Just l)
  Uint8   x _ -> Uint8   <$> accU8  "+" (+) x r <*> pure (Just l)
  Uint16  x _ -> Uint16  <$> accU16 "+" (+) x r <*> pure (Just l)
  Uint32  x _ -> Uint32  <$> accU32 "+" (+) x r <*> pure (Just l)
  Uint64  x _ -> Uint64  <$> accU64 "+" (+) x r <*> pure (Just l)
  Integer x _ -> Integer <$> accInteger "+" (+) x r <*> pure (Just l)
  _           -> raise $ TypeMismatch "+" "numeric" v

sub :: BuiltInFunc
-- arity mismatch
sub l [] = raise $ ArityMismatch l "-" 1 False 0
-- nenagte
sub l [v] = case v of
  Int8    x _ -> return $ Int8  (-x) $ Just l
  Int16   x _ -> return $ Int16 (-x) $ Just l
  Int32   x _ -> return $ Int32 (-x) $ Just l
  Int64   x _ -> return $ Int64 (-x) $ Just l
  Integer x _ -> return $ Integer (-x) $ Just l
  _           -> raise $ TypeMismatch "-" "numeric" v
-- subtraction
sub l (v:vs) = case v of
  Int8    x _ -> Int8    <$> accI8  "-" (-) x vs <*> pure (Just l)
  Int16   x _ -> Int16   <$> accI16 "-" (-) x vs <*> pure (Just l)
  Int32   x _ -> Int32   <$> accI32 "-" (-) x vs <*> pure (Just l)
  Int64   x _ -> Int64   <$> accI64 "-" (-) x vs <*> pure (Just l)
  Uint8   x _ -> Uint8   <$> accU8  "-" (-) x vs <*> pure (Just l)
  Uint16  x _ -> Uint16  <$> accU16 "-" (-) x vs <*> pure (Just l)
  Uint32  x _ -> Uint32  <$> accU32 "-" (-) x vs <*> pure (Just l)
  Uint64  x _ -> Uint64  <$> accU64 "-" (-) x vs <*> pure (Just l)
  Integer x _ -> Integer <$> accInteger "-" (-) x vs <*> pure (Just l)
  _           -> raise $ TypeMismatch "-" "numeric" v

mul :: BuiltInFunc
mul l [] = raise $ ArityMismatch l "*" 1 False 0
mul l (v:vs) = case v of
  Int8    x _ -> Int8   <$> accI8  "*" (*) x vs <*> pure (Just l)
  Int16   x _ -> Int16  <$> accI16 "*" (*) x vs <*> pure (Just l)
  Int32   x _ -> Int32  <$> accI32 "*" (*) x vs <*> pure (Just l)
  Int64   x _ -> Int64  <$> accI64 "*" (*) x vs <*> pure (Just l)
  Uint8   x _ -> Uint8  <$> accU8  "*" (*) x vs <*> pure (Just l)
  Uint16  x _ -> Uint16 <$> accU16 "*" (*) x vs <*> pure (Just l)
  Uint32  x _ -> Uint32 <$> accU32 "*" (*) x vs <*> pure (Just l)
  Uint64  x _ -> Uint64 <$> accU64 "*" (*) x vs <*> pure (Just l)
  Integer x _ -> Integer <$> accInteger "*" (*) x vs <*> pure (Just l)
  _           -> raise $ TypeMismatch "*" "numeric" v
