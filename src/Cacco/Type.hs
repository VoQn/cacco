module Cacco.Type where

import           Data.Monoid ((<>))

data Type
  = TyUnit
  | TyBool
  | TyInt8
  | TyInt16
  | TyInt32
  | TyInt64
  | TyInteger
  | TyUint8
  | TyUint16
  | TyUint32
  | TyUint64
  | TyNat
  | TyFloat16
  | TyFloat32
  | TyFloat64
  | TyDecimal
  | TyVar Int
  | TyFunc [Type] Type
  deriving Eq

instance Show Type where
  show TyUnit = "Unit"
  show TyBool = "Bool"

  show TyInt8    = "Int8"
  show TyInt16   = "Int16"
  show TyInt32   = "Int32"
  show TyInt64   = "Int64"
  show TyInteger = "Integer"

  show TyUint8  = "Uint8"
  show TyUint16 = "Uint16"
  show TyUint32 = "Uint32"
  show TyUint64 = "Uint64"
  show TyNat    = "Nat"

  show TyFloat16 = "Float16"
  show TyFloat32 = "Float32"
  show TyFloat64 = "Float64"
  show TyDecimal = "Decimal"

  show (TyVar i) = "t" <> show i

  show (TyFunc ps r) = pp ps <> " -> " <> show r
    where
      pp :: [Type] -> String
      pp [fun@(TyFunc _ _)]    = "(" <> show fun <> ")"
      pp (fun@(TyFunc _ _):ts) = "(" <> show fun <> ") -> " <> pp ts
      pp [t]                   = show t
      pp (t:ts)                = show t <> " -> " <> pp ts
      pp []                    = ""

data TypeSchema = TypeSchema [Int] Type
