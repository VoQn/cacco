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
  | TyWord8
  | TyWord16
  | TyWord32
  | TyWord64
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

  show TyWord8  = "Word8"
  show TyWord16 = "Word16"
  show TyWord32 = "Word32"
  show TyWord64 = "Word64"
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
      pp (t:ts)                = show t <> " -> " <> pp ts
      pp [t]                   = show t
      pp []                    = ""

data TypeSchema = TypeSchema [Int] Type
