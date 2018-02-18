module Cacco.Core
  ( Env, EvalF, BuiltInFunc
  , eq, neq
  , gt, gte, lt, lte
  , add, sub, mul
  )
  where

--
import           Data.Int
import           Data.Scientific (Scientific)
import           Data.Word
import           Numeric.Half    (Half)

import           Cacco.Error     (ArityMismatch (..), Error (..),
                                  TypeMismatch (..), printError)
import           Cacco.Location  (Location)
import           Cacco.Val       (Val (..))

type Env = [(String, Val Location)]
type EvalF = Env -> Either String (Val Location)

type BuiltInFunc = Location -> [Val Location] -> Either String (Val Location)

type Until a = String -> (a -> a -> Bool) -> a -> [Val Location] -> Either String Bool

raise :: Error e => e -> Either String a
raise = Left . printError
--
untilBool :: Until Bool
untilBool _ _ _ [] = return True
untilBool n f x (v:r) = case v of
  Bool y _ -> if f x y then return False else untilBool n f y r
  _        -> raise $ TypeMismatch n "Bool" v
--
untilI8 :: Until Int8
untilI8 _ _ _ [] = return True
untilI8 n f x (v:r) = case v of
  Int8 y _ -> if f x y then return False else untilI8 n f y r
  _        -> raise $ TypeMismatch n "Int8" v
--
untilI16 :: Until Int16
untilI16 _ _ _ [] = return True
untilI16 n f x (v:r) = case v of
  Int16 y _ -> if f x y then return False else untilI16 n f y r
  _         -> raise $ TypeMismatch n "Int16" v
--
untilI32 :: Until Int32
untilI32 _ _ _ [] = return True
untilI32 n f x (v:r) = case v of
  Int32 y _ -> if f x y then return False else untilI32 n f y r
  _         -> raise $ TypeMismatch n "Int32" v
--
untilI64 :: Until Int64
untilI64 _ _ _ [] = return True
untilI64 n f x (v:r) = case v of
  Int64 y _ -> if f x y then return False else untilI64 n f y r
  _         -> raise $ TypeMismatch n "Int64" v
--
untilU8 :: Until Word8
untilU8 _ _ _ [] = return True
untilU8 n f x (v:r) = case v of
  Uint8 y _ -> if f x y then return False else untilU8 n f y r
  _         -> raise $ TypeMismatch n "Uint8" v
--
untilU16 :: Until Word16
untilU16 _ _ _ [] = return True
untilU16 n f x (v:r) = case v of
  Uint16 y _ -> if f x y then return False else untilU16 n f y r
  _          -> raise $ TypeMismatch n "Uint16" v
--
untilU32 :: Until Word32
untilU32 _ _ _ [] = return True
untilU32 n f x (v:r) = case v of
  Uint32 y _ -> if f x y then return False else untilU32 n f y r
  _          -> raise $ TypeMismatch n "Uint32" v
--
untilU64 :: Until Word64
untilU64 _ _ _ [] = return True
untilU64 n f x (v:r) = case v of
  Uint64 y _ -> if f x y then return False else untilU64 n f y r
  _          -> raise $ TypeMismatch n "Uint64" v
--
untilInt :: Until Integer
untilInt _ _ _ [] = return True
untilInt n f x (v:r) = case v of
  Integer y _ -> if f x y then return False else untilInt n f y r
  _           -> raise $ TypeMismatch n "Integer" v
--
untilF16 :: Until Half
untilF16 _ _ _ [] = return True
untilF16 n f x (v:r) = case v of
  Float16 y _ -> if f x y then return False else untilF16 n f y r
  _           -> raise $ TypeMismatch n "Float16" v
--
untilF32 :: Until Float
untilF32 _ _ _ [] = return True
untilF32 n f x (v:r) = case v of
  Float32 y _ -> if f x y then return False else untilF32 n f y r
  _           -> raise $ TypeMismatch n "Float32" v
--
untilF64 :: Until Double
untilF64 _ _ _ [] = return True
untilF64 n f x (v:r) = case v of
  Float64 y _ -> if f x y then return False else untilF64 n f y r
  _           -> raise $ TypeMismatch n "Float64" v
--
untilFlo :: Until Scientific
untilFlo _ _ _ [] = return True
untilFlo n f x (v:r) = case v of
  Flonum y _ -> if f x y then return False else untilFlo n f y r
  _          -> raise $ TypeMismatch n "Flonum" v
--

eqArityError :: Location -> Int -> ArityMismatch
eqArityError l = ArityMismatch l "==" 2 False

eq :: BuiltInFunc
eq l [] = raise $ eqArityError l 0
eq l [_] = raise $ eqArityError l 1
eq l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
    Bool    x _ -> untilBool `startWith` x
    Int8    x _ -> untilI8   `startWith` x
    Int16   x _ -> untilI16  `startWith` x
    Int32   x _ -> untilI32  `startWith` x
    Int64   x _ -> untilI64  `startWith` x
    Uint8   x _ -> untilU8   `startWith` x
    Uint16  x _ -> untilU16  `startWith` x
    Uint32  x _ -> untilU32  `startWith` x
    Uint64  x _ -> untilU64  `startWith` x
    Integer x _ -> untilInt  `startWith` x
    Float16 x _ -> untilF16  `startWith` x
    Float32 x _ -> untilF32  `startWith` x
    Float64 x _ -> untilF64  `startWith` x
    Flonum  x _ -> untilFlo  `startWith` x
    _           -> raise $ TypeMismatch "==" "Eq" v
  where
    startWith f x = f "==" (/=) x vs
    {-# INLINE startWith #-}

neq :: BuiltInFunc
neq l [] = raise $ ArityMismatch l "!=" 2 False 0
neq l [_] = raise $ ArityMismatch l "==" 2 False 1
neq l (v:vs) = (\b -> Bool b $ Just l) <$> case v of
    Bool    x _ -> untilBool `startWith` x
    Int8    x _ -> untilI8   `startWith` x
    Int16   x _ -> untilI16  `startWith` x
    Int32   x _ -> untilI32  `startWith` x
    Int64   x _ -> untilI64  `startWith` x
    Uint8   x _ -> untilU8   `startWith` x
    Uint16  x _ -> untilU16  `startWith` x
    Uint32  x _ -> untilU32  `startWith` x
    Uint64  x _ -> untilU64  `startWith` x
    Integer x _ -> untilInt  `startWith` x
    Float16 x _ -> untilF16  `startWith` x
    Float32 x _ -> untilF32  `startWith` x
    Float64 x _ -> untilF64  `startWith` x
    Flonum  x _ -> untilFlo  `startWith` x
    _           -> raise $ TypeMismatch "!=" "Eq" v
  where
    startWith f x = f "!=" (==) x vs
    {-# INLINE startWith #-}

gt :: BuiltInFunc
gt l [] = raise $ ArityMismatch l ">" 2 False 0
gt l [_] = raise $ ArityMismatch l ">" 2 False 1
gt l (v:vs) = ($ Just l) . Bool <$> case v of
    Int8    x _ -> untilI8  `startWith` x
    Int16   x _ -> untilI16 `startWith` x
    Int32   x _ -> untilI32 `startWith` x
    Int64   x _ -> untilI64 `startWith` x
    Uint8   x _ -> untilU8  `startWith` x
    Uint16  x _ -> untilU16 `startWith` x
    Uint32  x _ -> untilU32 `startWith` x
    Uint64  x _ -> untilU64 `startWith` x
    Integer x _ -> untilInt `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> raise $ TypeMismatch ">" "Ord" v
  where
    startWith f x = f ">" (<=) x vs
    {-# INLINE startWith #-}
--
gte :: BuiltInFunc
gte l [] = raise $ ArityMismatch l ">=" 2 False 0
gte l [_] = raise $ ArityMismatch l ">=" 2 False 0
gte l (v:vs) = ($ Just l) . Bool <$> case v of
    Int8    x _ -> untilI8  `startWith` x
    Int16   x _ -> untilI16 `startWith` x
    Int32   x _ -> untilI32 `startWith` x
    Int64   x _ -> untilI64 `startWith` x
    Uint8   x _ -> untilU8  `startWith` x
    Uint16  x _ -> untilU16 `startWith` x
    Uint32  x _ -> untilU32 `startWith` x
    Uint64  x _ -> untilU64 `startWith` x
    Integer x _ -> untilInt `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> raise $ TypeMismatch ">=" "Ord" v
  where
    startWith f x = f ">=" (<) x vs
    {-# INLINE startWith #-}
--
lt :: BuiltInFunc
lt l [] = raise $ ArityMismatch l "<" 2 False 0
lt l [_] = raise $ ArityMismatch l "<" 2 False 1
lt l (v:vs) = ($ Just l) . Bool <$> case v of
    Int8    x _ -> untilI8  `startWith` x
    Int16   x _ -> untilI16 `startWith` x
    Int32   x _ -> untilI32 `startWith` x
    Int64   x _ -> untilI64 `startWith` x
    Uint8   x _ -> untilU8  `startWith` x
    Uint16  x _ -> untilU16 `startWith` x
    Uint32  x _ -> untilU32 `startWith` x
    Uint64  x _ -> untilU64 `startWith` x
    Integer x _ -> untilInt `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> raise $ TypeMismatch "<" "Ord" v
  where
    startWith f x = f "<" (>=) x vs
    {-# INLINE startWith #-}
--
lte :: BuiltInFunc
lte l [] = raise $ ArityMismatch l "<=" 2 False 0
lte l [_] = raise $ ArityMismatch l "<=" 2 False 1
lte l (v:vs) = ($ Just l) . Bool <$> case v of
    Int8    x _ -> untilI8  `startWith` x
    Int16   x _ -> untilI16 `startWith` x
    Int32   x _ -> untilI32 `startWith` x
    Int64   x _ -> untilI64 `startWith` x
    Uint8   x _ -> untilU8  `startWith` x
    Uint16  x _ -> untilU16 `startWith` x
    Uint32  x _ -> untilU32 `startWith` x
    Uint64  x _ -> untilU64 `startWith` x
    Integer x _ -> untilInt `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> raise $ TypeMismatch "<=" "Ord" v
  where
    startWith f x = f "<=" (>) x vs
    {-# INLINE startWith #-}
--
type Acc a = String -> (a -> a -> a) -> a -> [Val Location] -> Either String a

accI8 :: Acc Int8
accI8 _ _ x []    = return x
accI8 n f x (v:r) = case v of
  Int8 y _ -> accI8 n f (f x y) r
  _        -> raise $ TypeMismatch n "Int8" v
--
accI16 :: Acc Int16
accI16 _ _ x []    = return x
accI16 n f x (v:r) = case v of
  Int16 y _ -> accI16 n f (f x y) r
  _         -> raise $ TypeMismatch n "Int16" v
--
accI32 :: Acc Int32
accI32 _ _ x []    = return x
accI32 n f x (v:r) = case v of
  Int32 y _ -> accI32 n f (f x y) r
  _         -> raise $ TypeMismatch n "Int32" v
--
accI64 :: Acc Int64
accI64 _ _ x []    = return x
accI64 n f x (v:r) = case v of
  Int64 y _ -> accI64 n f (f x y) r
  _         -> raise $ TypeMismatch n "Int64" v
--
accU8 :: Acc Word8
accU8 _ _ x []    = return x
accU8 n f x (v:r) = case v of
  Uint8 y _ -> accU8 n f (f x y) r
  _         -> raise $ TypeMismatch n "Uint8" v

accU16 :: Acc Word16
accU16 _ _ x []    = return x
accU16 n f x (v:r) = case v of
  Uint16 y _ -> accU16 n f (f x y) r
  _          -> raise $ TypeMismatch n "Uint16" v

accU32 :: Acc Word32
accU32 _ _ x []    = return x
accU32 n f x (v:r) = case v of
  Uint32 y _ -> accU32 n f (f x y) r
  _          -> raise $ TypeMismatch n "Uint32" v

accU64 :: Acc Word64
accU64 _ _ x []    = return x
accU64 n f x (v:r) = case v of
  Uint64 y _ -> accU64 n f (f x y) r
  _          -> raise $ TypeMismatch n "Uint64" v

accInt :: Acc Integer
accInt _ _ x []    = return x
accInt n f x (v:r) = case v of
  Integer y _ -> accInt n f (f x y) r
  _           -> raise $ TypeMismatch n "Integer" v
--
accF16 :: Acc Half
accF16 _ _ x []    = return x
accF16 n f x (v:r) = case v of
  Float16 y _ -> accF16 n f (f x y) r
  _           -> raise $ TypeMismatch n "Float16" v
--
accF32 :: Acc Float
accF32 _ _ x []    = return x
accF32 n f x (v:r) = case v of
  Float32 y _ -> accF32 n f (f x y) r
  _           -> raise $ TypeMismatch n "Float32" v
--
accF64 :: Acc Double
accF64 _ _ x []    = return x
accF64 n f x (v:r) = case v of
  Float64 y _ -> accF64 n f (f x y) r
  _           -> raise $ TypeMismatch n "Float64" v
--
accFlo :: Acc Scientific
accFlo _ _ x []    = return x
accFlo n f x (v:r) = case v of
  Flonum y _ -> accFlo n f (f x y) r
  _          -> raise $ TypeMismatch n "Flonum" v
--

add :: BuiltInFunc
add l []     = raise $ ArityMismatch l "+" 1 False 0
add l (v:vs) = ($ Just l) <$> case v of
    Int8    x _ -> Int8    <$> accI8  `startWith` x
    Int16   x _ -> Int16   <$> accI16 `startWith` x
    Int32   x _ -> Int32   <$> accI32 `startWith` x
    Int64   x _ -> Int64   <$> accI64 `startWith` x
    Uint8   x _ -> Uint8   <$> accU8  `startWith` x
    Uint16  x _ -> Uint16  <$> accU16 `startWith` x
    Uint32  x _ -> Uint32  <$> accU32 `startWith` x
    Uint64  x _ -> Uint64  <$> accU64 `startWith` x
    Integer x _ -> Integer <$> accInt `startWith` x
    Float16 x _ -> Float16 <$> accF16 `startWith` x
    Float32 x _ -> Float32 <$> accF32 `startWith` x
    Float64 x _ -> Float64 <$> accF64 `startWith` x
    Flonum  x _ -> Flonum  <$> accFlo `startWith` x
    _           -> raise $ TypeMismatch "+" "numeric" v
  where
    startWith f x = f "+" (+) x vs
    {-# INLINE startWith #-}

sub :: BuiltInFunc
-- arity mismatch
sub l [] = raise $ ArityMismatch l "-" 1 False 0
-- nenagte
sub l [v] = ($ Just l) <$> case v of
  Int8    x _ -> Int8    <$> pure (negate x)
  Int16   x _ -> Int16   <$> pure (negate x)
  Int32   x _ -> Int32   <$> pure (negate x)
  Int64   x _ -> Int64   <$> pure (negate x)
  Integer x _ -> Integer <$> pure (negate x)
  Float16 x _ -> Float16 <$> pure (negate x)
  Float32 x _ -> Float32 <$> pure (negate x)
  Float64 x _ -> Float64 <$> pure (negate x)
  Flonum  x _ -> Flonum  <$> pure (negate x)
  _           -> raise $ TypeMismatch "-" "numeric" v
-- subtraction
sub l (v:vs) = ($ Just l) <$> case v of
    Int8    x _ -> Int8    <$> accI8  `startWith` x
    Int16   x _ -> Int16   <$> accI16 `startWith` x
    Int32   x _ -> Int32   <$> accI32 `startWith` x
    Int64   x _ -> Int64   <$> accI64 `startWith` x
    Uint8   x _ -> Uint8   <$> accU8  `startWith` x
    Uint16  x _ -> Uint16  <$> accU16 `startWith` x
    Uint32  x _ -> Uint32  <$> accU32 `startWith` x
    Uint64  x _ -> Uint64  <$> accU64 `startWith` x
    Integer x _ -> Integer <$> accInt `startWith` x
    Float16 x _ -> Float16 <$> accF16 `startWith` x
    Float32 x _ -> Float32 <$> accF32 `startWith` x
    Float64 x _ -> Float64 <$> accF64 `startWith` x
    Flonum  x _ -> Flonum  <$> accFlo `startWith` x
    _           -> raise $ TypeMismatch "-" "numeric" v
  where
    startWith f x = f "-" (-) x vs
    {-# INLINE startWith #-}

mul :: BuiltInFunc
mul l [] = raise $ ArityMismatch l "*" 1 False 0
mul l (v:vs) = ($ Just l) <$> case v of
    Int8    x _ -> Int8    <$> accI8  `startWith` x
    Int16   x _ -> Int16   <$> accI16 `startWith` x
    Int32   x _ -> Int32   <$> accI32 `startWith` x
    Int64   x _ -> Int64   <$> accI64 `startWith` x
    Uint8   x _ -> Uint8   <$> accU8  `startWith` x
    Uint16  x _ -> Uint16  <$> accU16 `startWith` x
    Uint32  x _ -> Uint32  <$> accU32 `startWith` x
    Uint64  x _ -> Uint64  <$> accU64 `startWith` x
    Integer x _ -> Integer <$> accInt `startWith` x
    Float16 x _ -> Float16 <$> accF16 `startWith` x
    Float32 x _ -> Float32 <$> accF32 `startWith` x
    Float64 x _ -> Float64 <$> accF64 `startWith` x
    Flonum  x _ -> Flonum  <$> accFlo `startWith` x
    _           -> raise $ TypeMismatch "*" "numeric" v
  where
    startWith f x = f "*" (*) x vs
    {-# INLINE startWith #-}
