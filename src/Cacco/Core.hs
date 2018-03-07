{-# LANGUAGE ScopedTypeVariables #-}

module Cacco.Core
  ( Env
  , eq, ne
  , gt, ge, lt, le
  , add, sub, mul
  , builtin
  )
  where

import           Control.Lens              hiding (List)
import           Data.Int
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Scientific           (Scientific)
import qualified Data.Text                 as Text
import           Data.Word
import           Numeric.Half              (Half)
import           Numeric.Natural

import           Cacco.Error               (Error (..))
import qualified Cacco.Error.ArityMismatch as AME
import qualified Cacco.Error.TypeMismatch  as TME
import           Cacco.Val                 (Fn, Val (..), pretty)
import qualified Cacco.Val                 as Val

type Env i = Map String (Val i)

type Until a i = String -> (a -> a -> Bool) -> a -> [Val i] -> Either (Error i) Bool

type Acc a i = String -> (a -> a -> a) -> a -> [Val i] -> Either (Error i) (Val i)

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

untilBool :: Until Bool i
untilBool name cond = go
  where
    go _ []             = return True
    go x (Bool y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Bool" unexpected

untilI8 :: Until Int8 i
untilI8 name cond = go
  where
    go _ []             = return True
    go x (Int8 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Int8" unexpected

untilI16 :: Until Int16 i
untilI16 name cond = go
  where
    go _ []             = return True
    go x (Int16 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Int16" unexpected

untilI32 :: Until Int32 i
untilI32 name cond = go
  where
    go _ []             = return True
    go x (Int32 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Int32" unexpected

untilI64 :: Until Int64 i
untilI64 name cond = go
  where
    go _ []             = return True
    go x (Int64 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Int64" unexpected

untilU8 :: Until Word8 i
untilU8 name cond = go
  where
    go _ []             = return True
    go x (Uint8 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Uint8" unexpected

untilU16 :: Until Word16 i
untilU16 name cond = go
  where
    go _ []             = return True
    go x (Uint16 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Uint16" unexpected

untilU32 :: Until Word32 i
untilU32 name cond = go
  where
    go _ []             = return True
    go x (Uint32 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Uint32" unexpected

untilU64 :: Until Word64 i
untilU64 name cond = go
  where
    go _ []             = return True
    go x (Uint64 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Uint64" unexpected

untilInt :: Until Integer i
untilInt name cond = go
  where
    go _ []             = return True
    go x (Integer y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Integer" unexpected

untilNat :: Until Natural i
untilNat name cond = go
  where
    go _ []             = return True
    go x (Natural y _:rest)
        | cond x y      = return False
        | otherwise     = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Numeric" unexpected

untilF16 :: Until Half i
untilF16 name cond = go
  where
    go _ []             = return True
    go x (Float16 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Float16" unexpected

untilF32 :: Until Float i
untilF32 name cond = go
  where
    go _ []             = return True
    go x (Float32 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Float32" unexpected

untilF64 :: Until Double i
untilF64 name cond = go
  where
    go _ []             = return True
    go x (Float64 y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Float64" unexpected

untilFlo :: Until Scientific i
untilFlo name cond = go
  where
    go _ []             = return True
    go x (Flonum y _:rest)
      | cond x y        = return False
      | otherwise       = go y rest
    go _ (unexpected:_) = Left $ typeMismatch name "Flonum" unexpected

eq :: forall i. Fn i
eq []           = Left $ arityMismatch "==" 2 True 0
eq [_]          = Left $ arityMismatch "==" 2 True 1
eq (value:rest) = Val.bool <$> case value of
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
    Natural x _ -> untilNat  `startWith` x
    Float16 x _ -> untilF16  `startWith` x
    Float32 x _ -> untilF32  `startWith` x
    Float64 x _ -> untilF64  `startWith` x
    Flonum  x _ -> untilFlo  `startWith` x
    _           -> Left $ typeMismatch "==" "Eq" value
  where
    startWith :: Eq a => Until a i -> a -> Either (Error i) Bool
    startWith run ini = run "==" (/=) ini rest
    {-# INLINE startWith #-}

ne :: forall i. Fn i
ne [] = Left $ arityMismatch "!=" 2 True 0
ne [_] = Left $ arityMismatch "==" 2 True 1
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
    Natural x _ -> untilNat  `startWith` x
    Float16 x _ -> untilF16  `startWith` x
    Float32 x _ -> untilF32  `startWith` x
    Float64 x _ -> untilF64  `startWith` x
    Flonum  x _ -> untilFlo  `startWith` x
    _           -> Left $ typeMismatch "!=" "Eq" v
  where
    startWith :: Eq a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f "!=" (==) x vs
    {-# INLINE startWith #-}

gt :: forall i. Fn i
gt [] = Left $ arityMismatch ">" 2 True 0
gt [_] = Left $ arityMismatch ">" 2 True 1
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
    Natural x _ -> untilNat `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> Left $ typeMismatch ">" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f ">" (<=) x vs
    {-# INLINE startWith #-}

ge :: forall i. Fn i
ge [] = Left $ arityMismatch ">=" 2 True 0
ge [_] = Left $ arityMismatch ">=" 2 True 0
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
    Natural x _ -> untilNat `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> Left $ typeMismatch ">=" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f ">=" (<) x vs
    {-# INLINE startWith #-}

lt :: forall i. Fn i
lt [] = Left $ arityMismatch "<" 2 True 0
lt [_] = Left $ arityMismatch "<" 2 True 1
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
    Natural x _ -> untilNat `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> Left $ typeMismatch "<" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f "<" (>=) x vs
    {-# INLINE startWith #-}

le :: forall i. Fn i
le [] = Left $ arityMismatch "<=" 2 True 0
le [_] = Left $ arityMismatch "<=" 2 True 1
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
    Natural x _ -> untilNat `startWith` x
    Float16 x _ -> untilF16 `startWith` x
    Float32 x _ -> untilF32 `startWith` x
    Float64 x _ -> untilF64 `startWith` x
    Flonum  x _ -> untilFlo `startWith` x
    _           -> Left $ typeMismatch "<=" "Ord" v
  where
    startWith :: Ord a => Until a i -> a -> Either (Error i) Bool
    startWith f x = f "<=" (>) x vs
    {-# INLINE startWith #-}

accI8 :: Acc Int8 i
accI8 name bin = go
  where
    go x []              = return $ Val.int8 x
    go x (Int8 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)  = Left $ typeMismatch name "Int8" unexpected

accI16 :: Acc Int16 i
accI16 name bin = go
  where
    go x []               = return $ Val.int16 x
    go x (Int16 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)   = Left $ typeMismatch name "Int16" unexpected

accI32 :: Acc Int32 i
accI32 name bin = go
  where
    go x []               = return $ Val.int32 x
    go x (Int32 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)   = Left $ typeMismatch name "Int32" unexpected

accI64 :: Acc Int64 i
accI64 name bin = go
  where
    go x []               = return $ Val.int64 x
    go x (Int64 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)   = Left $ typeMismatch name "Int64" unexpected

accU8 :: Acc Word8 i
accU8 name bin = go
  where
    go x []               = return $ Val.uint8 x
    go x (Uint8 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)   = Left $ typeMismatch name "Uint8" unexpected

accU16 :: Acc Word16 i
accU16 name bin = go
  where
    go x []                = return $ Val.uint16 x
    go x (Uint16 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)    = Left $ typeMismatch name "Uint16" unexpected

accU32 :: Acc Word32 i
accU32 name bin = go
  where
    go x []                = return $ Val.uint32 x
    go x (Uint32 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)    = Left $ typeMismatch name "Uint32" unexpected

accU64 :: Acc Word64 i
accU64 name bin = go
  where
    go x []                = return $ Val.uint64 x
    go x (Uint64 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)    = Left $ typeMismatch name "Uint64" unexpected

accInt :: Acc Integer i
accInt name bin = go
  where
    go x []                 = return $ Val.integer x
    go x (Integer y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)     = Left $ typeMismatch name "Integer" unexpected

accNat :: Acc Natural i
accNat name bin = go
  where
    go x []                 = return $ Val.natural x
    go x (Natural y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)     = Left $ typeMismatch name "Numeric" unexpected

accF16 :: Acc Half i
accF16 name bin = go
  where
    go x []                 = return $ Val.float16 x
    go x (Float16 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)     = Left $ typeMismatch name "Float16" unexpected

accF32 :: Acc Float i
accF32 name bin = go
  where
    go x []                 = return $ Val.float32 x
    go x (Float32 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)     = Left $ typeMismatch name "Float32" unexpected

accF64 :: Acc Double i
accF64 name bin = go
  where
    go x []                 = return $ Val.float64 x
    go x (Float64 y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)     = Left $ typeMismatch name "Float64" unexpected

accFlo :: Acc Scientific i
accFlo name bin = go
  where
    go x []                = return $ Val.flonum x
    go x (Flonum y _:rest) = go (x `bin` y) rest
    go _ (unexpected:_)    = Left $ typeMismatch name "Flonum" unexpected

add :: forall i. Fn i
add []     = Left $ arityMismatch "+" 1 True 0
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
    Natural x _ -> accNat `startWith` x
    Float16 x _ -> accF16 `startWith` x
    Float32 x _ -> accF32 `startWith` x
    Float64 x _ -> accF64 `startWith` x
    Flonum  x _ -> accFlo `startWith` x
    _           -> Left $ typeMismatch "+" "numeric" v
  where
    startWith :: Num a => Acc a i -> a -> Either (Error i) (Val i)
    startWith f x = f "+" (+) x vs
    {-# INLINE startWith #-}

sub :: forall i. Fn i
-- arity mismatch
sub [] = Left $ arityMismatch "-" 1 True 0
-- nenagte
sub [v] = ($ Nothing) <$> case v of
  Int8    x _ -> Int8    <$> pure (negate x)
  Int16   x _ -> Int16   <$> pure (negate x)
  Int32   x _ -> Int32   <$> pure (negate x)
  Int64   x _ -> Int64   <$> pure (negate x)
  Integer x _ -> Integer <$> pure (negate x)
  Natural x _ -> Integer <$> pure (negate $ fromIntegral x)
  Float16 x _ -> Float16 <$> pure (negate x)
  Float32 x _ -> Float32 <$> pure (negate x)
  Float64 x _ -> Float64 <$> pure (negate x)
  Flonum  x _ -> Flonum  <$> pure (negate x)
  _           -> Left $ typeMismatch "-" "numeric" v
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
    Natural x _ -> accNat `startWith` x
    Float16 x _ -> accF16 `startWith` x
    Float32 x _ -> accF32 `startWith` x
    Float64 x _ -> accF64 `startWith` x
    Flonum  x _ -> accFlo `startWith` x
    _           -> Left $ typeMismatch "-" "numeric" v
  where
    startWith :: Num a => Acc a i -> a -> Either (Error i) (Val i)
    startWith f x = f "-" (-) x vs
    {-# INLINE startWith #-}

mul :: forall i. Fn i
mul [] = Left $ arityMismatch "*" 1 True 0
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
    Natural x _ -> accNat `startWith` x
    Float16 x _ -> accF16 `startWith` x
    Float32 x _ -> accF32 `startWith` x
    Float64 x _ -> accF64 `startWith` x
    Flonum  x _ -> accFlo `startWith` x
    _           -> Left $ typeMismatch "*" "numeric" v
  where
    startWith :: Num a => Acc a i -> a -> Either (Error i) (Val i)
    startWith f x = f "*" (*) x vs
    {-# INLINE startWith #-}

len :: Fn i
len [List xs _]  = return $ Val.integer $ fromIntegral $ length xs
len [Text tx _]  = return $ Val.integer $ fromIntegral $ Text.length tx
len [unexpected] = Left $ typeMismatch "len" "Sequence" unexpected
len vs           = Left $ arityMismatch "len" 1 False $ length vs

lis :: Fn i
lis vs = Val.list <$> pure vs

head' :: Fn i
head' [List [] _]  = Left $ Message "empty" Nothing
head' [List xs _]  = return $ head xs
head' [Text tx _]
  | Text.null tx   = Left $ Message "empty" Nothing
  | otherwise      = return $ Val.text $ Text.singleton $ Text.head tx
head' [unexpected] = Left $ typeMismatch "head" "Sequence" unexpected
head' vs           = Left $ arityMismatch "head" 1 False $ length vs

tail' :: Fn i
tail' [List [] _]  = Left $ Message "empty" Nothing
tail' [List xs _]  = return $ Val.list $ tail xs
tail' [Text tx _]
  | Text.null tx   = Left $ Message "empty" Nothing
  | otherwise      = return $ Val.text $ Text.tail tx
tail' [unexpected] = Left $ typeMismatch "tail" "Sequence" unexpected
tail' vs           = Left $ arityMismatch "tail" 1 False $ length vs

isNull :: Fn i
isNull [List xs _]  = return $ Val.bool $ null xs
isNull [Text tx _]  = return $ Val.bool $ Text.null tx
isNull [unexpected] = Left $ typeMismatch "null?" "Collection" unexpected
isNull vs           = Left $ arityMismatch "null?" 1 False $ length vs

builtin :: Env i
builtin = Map.fromList $ wrap <$>
    [ ("==", eq), ("!=", ne)
    , (">",  gt), (">=", ge), ("<",  lt), ("<=", le)
    , ("+",  add), ("-",  sub), ("*",  mul)
    , ("null?", isNull), ("len",  len), ("list", lis), ("head", head'), ("tail", tail')
    ]
  where
    wrap (k, f) = (k, Builtin f Nothing)
