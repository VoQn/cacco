{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Cacco.Literal
  ( Literal(..)

  ) where

import           Data.Int        (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           Data.Word       (Word16, Word32, Word64, Word8)
import           GHC.Generics    (Generic)
import           Numeric.Half    (Half)

data Literal
  = Undef
  | Unit
  | Bool !Bool

  -- Signed integers
  | Int8    !Int8
  | Int16   !Int16
  | Int32   !Int32
  | Int64   !Int64
  | Integer !Integer

  -- Unsigned integers
  | Uint8  !Word8
  | Uint16 !Word16
  | Uint32 !Word32
  | Uint64 !Word64

  -- Floating point numbers
  | Float16 !Half
  | Float32 !Float
  | Float64 !Double
  | Flonum  !Scientific

  -- Text
  | Text !Text
  deriving (Eq, Ord, Show, Typeable, Generic)
