{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cacco.Val
  ( Val(..)
  , Fn
  , unit, bool
  , int8, int16, int32, int64
  , uint8, uint16, uint32, uint64
  , integer
  , float16, float32, float64, flonum
  , info
  , setInfo
  , removeInfo
  , pretty
  ) where

import           Data.Int        (Int16, Int32, Int64, Int8)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vec
import           Data.Word       (Word16, Word32, Word64, Word8)
import           GHC.Generics    (Generic)
import           Numeric.Half    (Half)

data Val i
  = Unit (Maybe i)

  | Bool !Bool (Maybe i)

  | Int8    !Int8    (Maybe i)
  | Int16   !Int16   (Maybe i)
  | Int32   !Int32   (Maybe i)
  | Int64   !Int64   (Maybe i)
  | Uint8   !Word8   (Maybe i)
  | Uint16  !Word16  (Maybe i)
  | Uint32  !Word32  (Maybe i)
  | Uint64  !Word64  (Maybe i)
  | Integer !Integer (Maybe i)

  | Float16 !Half       (Maybe i)
  | Float32 !Float      (Maybe i)
  | Float64 !Double     (Maybe i)
  | Flonum  !Scientific (Maybe i)

  | Text !Text (Maybe i)

  | Symbol !String (Maybe i)

  | List [Val i] (Maybe i)
  | Vector (Vector (Val i)) (Maybe i)
  | Struct (Map String (Val i)) (Maybe i)

  | BultIn (Fn i) (Maybe i)
  | Func (Fn i) (Maybe i)
  deriving (Typeable, Generic)

type Fn i = [Val i] -> Either String (Val i)

unit :: Val i
unit = Unit Nothing

bool :: Bool -> Val i
bool b = Bool b Nothing

int8 :: Int8 -> Val i
int8 x = Int8 x Nothing

int16 :: Int16 -> Val i
int16 x = Int16 x Nothing

int32 :: Int32 -> Val i
int32 x = Int32 x Nothing

int64 :: Int64 -> Val i
int64 x = Int64 x Nothing

uint8 :: Word8 -> Val i
uint8 x = Uint8 x Nothing

uint16 :: Word16 -> Val i
uint16 x = Uint16 x Nothing

uint32 :: Word32 -> Val i
uint32 x = Uint32 x Nothing

uint64 :: Word64 -> Val i
uint64 x = Uint64 x Nothing

integer :: Integer -> Val i
integer x = Integer x Nothing

float16 :: Half -> Val i
float16 x = Float16 x Nothing

float32 :: Float -> Val i
float32 x = Float32 x Nothing

float64 :: Double -> Val i
float64 x = Float64 x Nothing

flonum :: Scientific -> Val i
flonum x = Flonum x Nothing

instance Eq (Val a) where
  v1 == v2 = case (v1, v2) of
    (Unit      _, Unit      _) -> True
    (Bool    x _, Bool    y _) -> x == y
    (Int8    x _, Int8    y _) -> x == y
    (Int16   x _, Int16   y _) -> x == y
    (Int32   x _, Int32   y _) -> x == y
    (Int64   x _, Int64   y _) -> x == y
    (Uint8   x _, Uint8   y _) -> x == y
    (Uint16  x _, Uint16  y _) -> x == y
    (Uint32  x _, Uint32  y _) -> x == y
    (Uint64  x _, Uint64  y _) -> x == y
    (Integer x _, Integer y _) -> x == y
    (Float16 x _, Float16 y _) -> x == y
    (Float32 x _, Float32 y _) -> x == y
    (Float64 x _, Float64 y _) -> x == y
    (Flonum  x _, Flonum  y _) -> x == y
    (Symbol  x _, Symbol  y _) -> x == y
    (Text    x _, Text    y _) -> x == y
    (List    x _, List    y _) -> x == y
    (Vector  x _, Vector  y _) -> x == y
    (Struct  x _, Struct  y _) -> x == y
    _                          -> False

instance (Show a) => Show (Val a) where
  show val = case val of
    Unit      i -> unwords ["Unit", show i]
    Bool    x i -> unwords ["Bool", show x, show i]
    Int8    x i -> unwords ["Int8", show x, show i]
    Int16   x i -> unwords ["Int16", show x, show i]
    Int32   x i -> unwords ["Int32", show x, show i]
    Int64   x i -> unwords ["Int64", show x, show i]
    Uint8   x i -> unwords ["Uint8", show x, show i]
    Uint16  x i -> unwords ["Uint16", show x, show i]
    Uint32  x i -> unwords ["Uint32", show x, show i]
    Uint64  x i -> unwords ["Uint64", show x, show i]
    Integer x i -> unwords ["Integer", show x, show i]
    Float16 x i -> unwords ["Float16", show x, show i]
    Float32 x i -> unwords ["Float32", show x, show i]
    Float64 x i -> unwords ["Float64", show x, show i]
    Flonum  x i -> unwords ["Flonum", show x, show i]
    Text    x i -> unwords ["Text", show x, show i]
    Symbol  x i -> unwords ["Symbol", show x, show i]
    List    x i -> unwords ["List", show x, show i]
    Vector  x i -> unwords ["Vector", show x, show i]
    Struct  x i -> unwords ["Struct", show x, show i]
    BultIn  _ i -> unwords ["BultIn", show i]
    Func    _ i -> unwords ["Func", show i]

info :: Val i -> Maybe i
info val = case val of
  Unit      i -> i
  Bool    _ i -> i
  Int8    _ i -> i
  Int16   _ i -> i
  Int32   _ i -> i
  Int64   _ i -> i
  Uint8   _ i -> i
  Uint16  _ i -> i
  Uint32  _ i -> i
  Uint64  _ i -> i
  Integer _ i -> i
  Float16 _ i -> i
  Float32 _ i -> i
  Float64 _ i -> i
  Flonum  _ i -> i
  Text    _ i -> i
  Symbol  _ i -> i
  List    _ i -> i
  Vector  _ i -> i
  Struct  _ i -> i
  BultIn  _ i -> i
  Func    _ i -> i

setInfo :: (Maybe i) -> Val i -> Val i
setInfo i v = ($ i) $ case v of
  Unit      _ -> Unit
  Bool    x _ -> Bool x
  Int8    x _ -> Int8 x
  Int16   x _ -> Int16 x
  Int32   x _ -> Int32 x
  Int64   x _ -> Int64 x
  Uint8   x _ -> Uint8 x
  Uint16  x _ -> Uint16 x
  Uint32  x _ -> Uint32 x
  Uint64  x _ -> Uint64 x
  Integer x _ -> Integer x
  Float16 x _ -> Float16 x
  Float32 x _ -> Float32 x
  Float64 x _ -> Float64 x
  Flonum  x _ -> Flonum x
  Text    x _ -> Text x
  Symbol  x _ -> Symbol x
  List    x _ -> List x
  Vector  x _ -> Vector x
  Struct  x _ -> Struct x
  BultIn  x _ -> BultIn x
  Func    x _ -> Func x

removeInfo :: Val i -> Val i
removeInfo val = ($ Nothing) $ case val of
  Unit      _ -> Unit
  Bool    x _ -> Bool x
  Int8    x _ -> Int8 x
  Int16   x _ -> Int16 x
  Int32   x _ -> Int32 x
  Int64   x _ -> Int64 x
  Uint8   x _ -> Uint8 x
  Uint16  x _ -> Uint16 x
  Uint32  x _ -> Uint32 x
  Uint64  x _ -> Uint64 x
  Integer x _ -> Integer x
  Float16 x _ -> Float16 x
  Float32 x _ -> Float32 x
  Float64 x _ -> Float64 x
  Flonum  x _ -> Flonum x
  Text    x _ -> Text x
  Symbol  x _ -> Symbol x
  List    x _ -> List $ removeInfo <$> x
  Vector  x _ -> Vector $ removeInfo <$> x
  Struct  x _ -> Struct $ removeInfo <$> x
  BultIn  x _ -> BultIn x
  Func    x _ -> Func x

pretty :: Val i -> String
pretty val = case val of
  Unit      _ -> "()"
  Bool    x _ -> if x then "true" else "false"
  Int8    x _ -> show x
  Int16   x _ -> show x
  Int32   x _ -> show x
  Int64   x _ -> show x
  Uint8   x _ -> show x
  Uint16  x _ -> show x
  Uint32  x _ -> show x
  Uint64  x _ -> show x
  Integer x _ -> show x
  Float16 x _ -> show x
  Float32 x _ -> show x
  Float64 x _ -> show x
  Flonum  x _ -> show x
  Text    x _ -> show x
  Symbol  x _ -> x
  List    x _ -> '(' : unwords (pretty <$> x) ++ ")"
  Vector  x _ -> '[' : unwords (pretty <$> Vec.toList x) ++ "]"
  Struct  x _ -> '{' : unwords (prettyPair <$> Map.toList x) ++ "}"
  BultIn  _ _ -> "<#Bult_In#>"
  Func    _ _ -> "<#Function#>"

prettyPair :: (String, Val i) -> String
prettyPair (k, v) = k ++ ": " ++ pretty v
{-# INLINE prettyPair #-}
