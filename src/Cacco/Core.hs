{-# LANGUAGE ScopedTypeVariables #-}

module Cacco.Core
  ( Env, EvalF
  , eq, ne
  , gt, ge, lt, le
  , add, sub, mul
  , builtin
  )
  where

--
import           Control.Lens
import           Data.Int
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Scientific           (Scientific)
import           Data.Word
import           Numeric.Half              (Half)


import           Cacco.Error               (Error (..))
import qualified Cacco.Error.ArityMismatch as AME
import qualified Cacco.Error.TypeMismatch  as TME
import           Cacco.Val                 (Fn, Val (..), pretty)
import qualified Cacco.Val                 as Val

type Env i = Map String (Val i)
type EvalF i = Env i -> Either (Error i) (Val i)

type Until a i = String -> (a -> a -> Bool) -> a -> [Val i] -> Either (Error i) Bool

typeMismatch :: String -> String -> Val i -> Error i
typeMismatch name expected val = TypeMismatch detail $ Val.info val
  where
    detail = TME.defaultError &
      TME.funcName .~ name &
      TME.expectedType .~ expected &
      TME.applied .~ pretty val

arityMismatch :: String -> Int -> Bool -> Int -> Error i
arityMismatch name arity variadic actual = ArityMismatch detail Nothing
  where
    detail = AME.defaultError &
      AME.funcName .~ name &
      AME.arity .~ arity &
      AME.isVariadic .~ variadic &
      AME.applied .~ actual

raise :: Error i -> Either (Error i) a
raise = Left
--
untilBool :: Show i => Until Bool i
untilBool _ _ _ [] = return True
untilBool n f x (v:r) = case v of
  Bool y _ -> if f x y then return False else untilBool n f y r
  _        -> raise $ typeMismatch n "Bool" v
--
untilI8 :: Show i => Until Int8 i
untilI8 _ _ _ [] = return True
untilI8 n f x (v:r) = case v of
  Int8 y _ -> if f x y then return False else untilI8 n f y r
  _        -> raise $ typeMismatch n "Int8" v
--
untilI16 :: Show i => Until Int16 i
untilI16 _ _ _ [] = return True
untilI16 n f x (v:r) = case v of
  Int16 y _ -> if f x y then return False else untilI16 n f y r
  _         -> raise $ typeMismatch n "Int16" v
--
untilI32 :: Show i => Until Int32 i
untilI32 _ _ _ [] = return True
untilI32 n f x (v:r) = case v of
  Int32 y _ -> if f x y then return False else untilI32 n f y r
  _         -> raise $ typeMismatch n "Int32" v
--
untilI64 :: Show i => Until Int64 i
untilI64 _ _ _ [] = return True
untilI64 n f x (v:r) = case v of
  Int64 y _ -> if f x y then return False else untilI64 n f y r
  _         -> raise $ typeMismatch n "Int64" v
--
untilU8 :: Show i => Until Word8 i
untilU8 _ _ _ [] = return True
untilU8 n f x (v:r) = case v of
  Uint8 y _ -> if f x y then return False else untilU8 n f y r
  _         -> raise $ typeMismatch n "Uint8" v
--
untilU16 :: Show i => Until Word16 i
untilU16 _ _ _ [] = return True
untilU16 n f x (v:r) = case v of
  Uint16 y _ -> if f x y then return False else untilU16 n f y r
  _          -> raise $ typeMismatch n "Uint16" v
--
untilU32 :: Show i => Until Word32 i
untilU32 _ _ _ [] = return True
untilU32 n f x (v:r) = case v of
  Uint32 y _ -> if f x y then return False else untilU32 n f y r
  _          -> raise $ typeMismatch n "Uint32" v
--
untilU64 :: Show i => Until Word64 i
untilU64 _ _ _ [] = return True
untilU64 n f x (v:r) = case v of
  Uint64 y _ -> if f x y then return False else untilU64 n f y r
  _          -> raise $ typeMismatch n "Uint64" v
--
untilInt :: Show i => Until Integer i
untilInt _ _ _ [] = return True
untilInt n f x (v:r) = case v of
  Integer y _ -> if f x y then return False else untilInt n f y r
  _           -> raise $ typeMismatch n "Integer" v
--
untilF16 :: Show i => Until Half i
untilF16 _ _ _ [] = return True
untilF16 n f x (v:r) = case v of
  Float16 y _ -> if f x y then return False else untilF16 n f y r
  _           -> raise $ typeMismatch n "Float16" v
--
untilF32 :: Show i => Until Float i
untilF32 _ _ _ [] = return True
untilF32 n f x (v:r) = case v of
  Float32 y _ -> if f x y then return False else untilF32 n f y r
  _           -> raise $ typeMismatch n "Float32" v
--
untilF64 :: Show i => Until Double i
untilF64 _ _ _ [] = return True
untilF64 n f x (v:r) = case v of
  Float64 y _ -> if f x y then return False else untilF64 n f y r
  _           -> raise $ typeMismatch n "Float64" v
--
untilFlo :: Show i => Until Scientific i
untilFlo _ _ _ [] = return True
untilFlo n f x (v:r) = case v of
  Flonum y _ -> if f x y then return False else untilFlo n f y r
  _          -> raise $ typeMismatch n "Flonum" v
--

eq :: forall i. Show i => Fn i
eq [] = raise $ arityMismatch "==" 2 False 0
eq [_] = raise $ arityMismatch "==" 2 False 1
eq (v:vs) = Val.bool <$> case v of
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
    _           -> raise $ typeMismatch "==" "Eq" v
  where
    startWith :: Eq a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f "==" (/=) x vs
    {-# INLINE startWith #-}

ne :: forall i. Show i => Fn i
ne [] = raise $ arityMismatch "!=" 2 False 0
ne [_] = raise $ arityMismatch "==" 2 False 1
ne (v:vs) = Val.bool <$> case v of
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
    _           -> raise $ typeMismatch "!=" "Eq" v
  where
    startWith :: Eq a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f "!=" (==) x vs
    {-# INLINE startWith #-}

gt :: forall i. Show i => Fn i
gt [] = raise $ arityMismatch ">" 2 False 0
gt [_] = raise $ arityMismatch ">" 2 False 1
gt (v:vs) = Val.bool <$> case v of
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
    _           -> raise $ typeMismatch ">" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f ">" (<=) x vs
    {-# INLINE startWith #-}
--
ge :: forall i. Show i => Fn i
ge [] = raise $ arityMismatch ">=" 2 False 0
ge [_] = raise $ arityMismatch ">=" 2 False 0
ge (v:vs) = Val.bool <$> case v of
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
    _           -> raise $ typeMismatch ">=" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f ">=" (<) x vs
    {-# INLINE startWith #-}
--
lt :: forall i. Show i => Fn i
lt [] = raise $ arityMismatch "<" 2 False 0
lt [_] = raise $ arityMismatch "<" 2 False 1
lt (v:vs) = Val.bool <$> case v of
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
    _           -> raise $ typeMismatch "<" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f "<" (>=) x vs
    {-# INLINE startWith #-}
--
le :: forall i. Show i => Fn i
le [] = raise $ arityMismatch "<=" 2 False 0
le [_] = raise $ arityMismatch "<=" 2 False 1
le (v:vs) = Val.bool <$> case v of
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
    _           -> raise $ typeMismatch "<=" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f "<=" (>) x vs
    {-# INLINE startWith #-}
--

type Acc a i = String -> (a -> a -> a) -> a -> [Val i] -> Either (Error i) (Val i)

accI8 :: Show i => Acc Int8 i
accI8 _ _ x []    = return $ Val.int8 x
accI8 n f x (v:r) = case v of
  Int8 y _ -> accI8 n f (f x y) r
  _        -> raise $ typeMismatch n "Int8" v
--
accI16 :: Show i => Acc Int16 i
accI16 _ _ x []    = return $ Val.int16 x
accI16 n f x (v:r) = case v of
  Int16 y _ -> accI16 n f (f x y) r
  _         -> raise $ typeMismatch n "Int16" v
--
accI32 :: Show i => Acc Int32 i
accI32 _ _ x []    = return $ Val.int32 x
accI32 n f x (v:r) = case v of
  Int32 y _ -> accI32 n f (f x y) r
  _         -> raise $ typeMismatch n "Int32" v
--
accI64 :: Show i => Acc Int64 i
accI64 _ _ x []    = return $ Val.int64 x
accI64 n f x (v:r) = case v of
  Int64 y _ -> accI64 n f (f x y) r
  _         -> raise $ typeMismatch n "Int64" v
--
accU8 :: Show i => Acc Word8 i
accU8 _ _ x []    = return $ Val.uint8 x
accU8 n f x (v:r) = case v of
  Uint8 y _ -> accU8 n f (f x y) r
  _         -> raise $ typeMismatch n "Uint8" v

accU16 :: Show i => Acc Word16 i
accU16 _ _ x []    = return $ Val.uint16 x
accU16 n f x (v:r) = case v of
  Uint16 y _ -> accU16 n f (f x y) r
  _          -> raise $ typeMismatch n "Uint16" v

accU32 :: Show i => Acc Word32 i
accU32 _ _ x []    = return $ Val.uint32 x
accU32 n f x (v:r) = case v of
  Uint32 y _ -> accU32 n f (f x y) r
  _          -> raise $ typeMismatch n "Uint32" v

accU64 :: Show i => Acc Word64 i
accU64 _ _ x []    = return $ Val.uint64 x
accU64 n f x (v:r) = case v of
  Uint64 y _ -> accU64 n f (f x y) r
  _          -> raise $ typeMismatch n "Uint64" v

accInt :: Show i => Acc Integer i
accInt _ _ x []    = return $ Val.integer x
accInt n f x (v:r) = case v of
  Integer y _ -> accInt n f (f x y) r
  _           -> raise $ typeMismatch n "Integer" v
--
accF16 :: Show i => Acc Half i
accF16 _ _ x []    = return $ Val.float16 x
accF16 n f x (v:r) = case v of
  Float16 y _ -> accF16 n f (f x y) r
  _           -> raise $ typeMismatch n "Float16" v
--
accF32 :: Show i => Acc Float i
accF32 _ _ x []    = return $ Val.float32 x
accF32 n f x (v:r) = case v of
  Float32 y _ -> accF32 n f (f x y) r
  _           -> raise $ typeMismatch n "Float32" v
--
accF64 :: Show i => Acc Double i
accF64 _ _ x []    = return $ Val.float64 x
accF64 n f x (v:r) = case v of
  Float64 y _ -> accF64 n f (f x y) r
  _           -> raise $ typeMismatch n "Float64" v
--
accFlo :: Show i => Acc Scientific i
accFlo _ _ x []    = return $ Val.flonum x
accFlo n f x (v:r) = case v of
  Flonum y _ -> accFlo n f (f x y) r
  _          -> raise $ typeMismatch n "Flonum" v
--

--
add :: forall i. Show i => Fn i
add []     = raise $ arityMismatch "+" 1 False 0
add (v:vs) = case v of
    Int8    x _ -> accI8  `startWith` x
    Int16   x _ -> accI16 `startWith` x
    Int32   x _ -> accI32 `startWith` x
    Int64   x _ -> accI64 `startWith` x
    Uint8   x _ -> accU8  `startWith` x
    Uint16  x _ -> accU16 `startWith` x
    Uint32  x _ -> accU32 `startWith` x
    Uint64  x _ -> accU64 `startWith` x
    Integer x _ -> accInt `startWith` x
    Float16 x _ -> accF16 `startWith` x
    Float32 x _ -> accF32 `startWith` x
    Float64 x _ -> accF64 `startWith` x
    Flonum  x _ -> accFlo `startWith` x
    _           -> raise $ typeMismatch "+" "numeric" v
  where
    startWith :: Num a => Acc a i -> a -> Either (Error i) (Val i)
    startWith f x = f "+" (+) x vs
    {-# INLINE startWith #-}
--
sub :: forall i. Show i => Fn i
-- arity mismatch
sub [] = raise $ arityMismatch "-" 1 False 0
-- nenagte
sub [v] = ($ Nothing) <$> case v of
  Int8    x _ -> Int8    <$> pure (negate x)
  Int16   x _ -> Int16   <$> pure (negate x)
  Int32   x _ -> Int32   <$> pure (negate x)
  Int64   x _ -> Int64   <$> pure (negate x)
  Integer x _ -> Integer <$> pure (negate x)
  Float16 x _ -> Float16 <$> pure (negate x)
  Float32 x _ -> Float32 <$> pure (negate x)
  Float64 x _ -> Float64 <$> pure (negate x)
  Flonum  x _ -> Flonum  <$> pure (negate x)
  _           -> raise $ typeMismatch "-" "numeric" v
-- subtraction
sub (v:vs) = case v of
    Int8    x _ -> accI8  `startWith` x
    Int16   x _ -> accI16 `startWith` x
    Int32   x _ -> accI32 `startWith` x
    Int64   x _ -> accI64 `startWith` x
    Uint8   x _ -> accU8  `startWith` x
    Uint16  x _ -> accU16 `startWith` x
    Uint32  x _ -> accU32 `startWith` x
    Uint64  x _ -> accU64 `startWith` x
    Integer x _ -> accInt `startWith` x
    Float16 x _ -> accF16 `startWith` x
    Float32 x _ -> accF32 `startWith` x
    Float64 x _ -> accF64 `startWith` x
    Flonum  x _ -> accFlo `startWith` x
    _           -> raise $ typeMismatch "-" "numeric" v
  where
    startWith :: Num a => Acc a i -> a -> Either (Error i) (Val i)
    startWith f x = f "-" (-) x vs
    {-# INLINE startWith #-}

mul :: forall i. Show i => Fn i
mul [] = raise $ arityMismatch "*" 1 False 0
mul (v:vs) = case v of
    Int8    x _ -> accI8  `startWith` x
    Int16   x _ -> accI16 `startWith` x
    Int32   x _ -> accI32 `startWith` x
    Int64   x _ -> accI64 `startWith` x
    Uint8   x _ -> accU8  `startWith` x
    Uint16  x _ -> accU16 `startWith` x
    Uint32  x _ -> accU32 `startWith` x
    Uint64  x _ -> accU64 `startWith` x
    Integer x _ -> accInt `startWith` x
    Float16 x _ -> accF16 `startWith` x
    Float32 x _ -> accF32 `startWith` x
    Float64 x _ -> accF64 `startWith` x
    Flonum  x _ -> accFlo `startWith` x
    _           -> raise $ typeMismatch "*" "numeric" v
  where
    startWith :: Num a => Acc a i -> a -> Either (Error i) (Val i)
    startWith f x = f "*" (*) x vs
    {-# INLINE startWith #-}

builtin :: Show i => Env i
builtin = Map.fromList $ wrap <$>
    [ ("==", eq)
    , ("!=", ne)
    , (">",  gt)
    , (">=", ge)
    , ("<",  lt)
    , ("<=", le)
    , ("+",  add)
    , ("-",  sub)
    , ("*",  mul)
    ]
  where
    wrap (k, f) = (k, Builtin f Nothing)
