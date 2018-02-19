{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.Parser.Numeric
  ( integer
  , flonum
  ) where

import           Data.Bits                    (shiftL)
import           Data.Functor                 (($>))
import           Data.List                    (foldl')
import           Data.Scientific              (Scientific, toRealFloat)
import           Text.Megaparsec              (choice, lookAhead, option, some,
                                               try, (<?>), (<|>))
import           Text.Megaparsec.Char         (char, digitChar)
import qualified Text.Megaparsec.Char.Lexer   as L

import           Cacco.Syntax.Literal
import           Cacco.Syntax.Parser.Internal (Parser)
import           Cacco.Syntax.Parser.Lexer

-- | Parse a number with optional sign
withSign :: Num a => Parser a -> Parser (Bool, a)
withSign parser = do
    (isSigned, fn) <- option (False, id) $ minus <|> plus
    number         <- parser
    return (isSigned, fn number)
  where
    minus :: Num a => Parser (Bool, a -> a)
    minus = char '-' $> (True, negate)
    {-# INLINE minus #-}

    plus :: Num a => Parser (Bool, a -> a)
    plus = char '+' $> (True, id)
    {-# INLINE plus #-}

-- | Parse a binary integer with prefix 'b' (e.g. 101010)
binary' :: Parser Integer
binary' = foldl' acc 0 <$> some (zero <|> one)
  where
    -- | Parse '0' as a boolean
    zero :: Parser Bool
    zero = char '0' $> False
    {-# INLINE zero #-}

    -- | Parse '1' as a boolean
    one :: Parser Bool
    one = char '1' $> True
    {-# INLINE one #-}

    -- | Accumulate boolean as bit-shift
    acc :: Integer -> Bool -> Integer
    acc 0 False = 0
    acc 0 True  = 1
    acc v False = v `shiftL` 1
    acc v True  = v `shiftL` 1 + 1

-- | Parse a integer with prefix for positional notation.
positional :: Parser Integer
positional = char '0' >> choice [hexadecimal, octal, binary]
  where
    -- | Parse a hexadecimal integer with prefix 'x' (e.g. xFA901)
    hexadecimal :: Parser Integer
    hexadecimal = char 'x' >> L.hexadecimal
    {-# INLINE hexadecimal #-}

    -- | Parse a octal integer with prefix 'o' (e.g. o080)
    octal :: Parser Integer
    octal = char 'o' >> L.octal
    {-# INLINE octal #-}

    -- | Parse a binary integer with prefix 'b' (e.g. b101010)
    binary :: Parser Integer
    binary = char 'b' >> binary'
    {-# INLINE binary #-}

integer :: Parser Literal
-- ^ Parse a integer number literal.
-- Integer literals:
--
-- [@unsigned-decimals@] @0@, @1@, @10@, ...
-- [@signed-decimals@] @-1@, @+10@, ...
-- [@octal@] @0o000@, @0o701@, ...
-- [@hexadecimal@] @0x00@, @0xFF@ ...
-- [@binary@] @0b0101@, ...
--
-- and /optional/ strict type suffix:
--
-- [@_i8@] 8bit integer
-- [@_u8@] 8bit unsigned-integer
-- [@_i16@] 16bit integer
-- [@_u16@] 16bit unsigned-integer
-- [@_i32@] 32bit integer
-- [@_u32@] 32bit unsigned-integer
-- [@_i64@] 64bit integer
-- [@_u64@] 64bit unsigned-integer
--
-- >>> parseTest integer "1"
-- Integer 1
--
-- >>> parseTest integer "0xFF"
-- Integer 256
--
-- >>> parseTest integer "1_i8"
-- Int8 1
--
-- >>> parseTest integer "1_u32"
-- Uint32 1
--
integer = integer' <?> "integer literal"

integer' :: Parser Literal
integer' = do
    (isSigned, number) <- try notated <|> withSign L.decimal
    wrapper            <- option Integer $ suffix isSigned
    return $ wrapper number
  where
    -- | parse positional notated integer
    notated :: Parser (Bool, Integer)
    notated = (,) False <$> positional
    {-# INLINE notated #-}

    -- | parse strict integer-type suffix
    suffix :: Bool -- ^ prefix was exist
           -> Parser (Integer -> Literal)
    suffix True  = char '_' >> signedInt
    suffix False = char '_' >> signedInt <|> unsignedInt

-- | parse signed-integer-type suffix
signedInt :: Parser (Integer -> Literal)
signedInt = char 'i' >> choice [i8, i16, i32, i64]
{-# INLINE signedInt #-}

-- | parse unsigned-integer-type suffix
unsignedInt :: Parser (Integer -> Literal)
unsignedInt = char 'u' >> choice [u8, u16, u32, u64]
{-# INLINE unsignedInt #-}

i8 :: Parser (Integer -> Literal)
i8  = symbol "8"  $> Int8  . fromInteger
{-# INLINE i8 #-}

i16 :: Parser (Integer -> Literal)
i16 = symbol "16" $> Int16 . fromInteger
{-# INLINE i16 #-}

i32 :: Parser (Integer -> Literal)
i32 = symbol "32" $> Int32 . fromInteger
{-# INLINE i32 #-}

i64 :: Parser (Integer -> Literal)
i64 = symbol "64" $> Int64 . fromInteger
{-# INLINE i64 #-}

u8 :: Parser (Integer -> Literal)
u8  = symbol "8"  $> Uint8  . fromInteger
{-# INLINE u8 #-}

u16 :: Parser (Integer -> Literal)
u16 = symbol "16" $> Uint16 . fromInteger
{-# INLINE u16 #-}

u32 :: Parser (Integer -> Literal)
u32 = symbol "32" $> Uint32 . fromInteger
{-# INLINE u32 #-}

u64 :: Parser (Integer -> Literal)
u64 = symbol "64" $> Uint64 . fromInteger
{-# INLINE u64 #-}

-- | Parse a floating point number.
flonum :: Parser Literal
flonum = float <?> "floating point number"
  where
    float :: Parser Literal
    float = do
      _       <- lookAhead $ try digitFloatFormat
      number  <- snd <$> withSign L.scientific
      wrapper <- option Flonum suffix
      return $ wrapper number

    suffix :: Parser (Scientific -> Literal)
    suffix = symbol "_f" >> choice [f16, f32, f64]

digitFloatFormat :: Parser Bool
digitFloatFormat = sign >> some digitChar >> (char '.' <|> char 'e') $> True
  where
    sign :: Parser ()
    sign = option () $ (char '-' <|> char '+') $> ()
    {-# INLINE sign #-}

f16 :: Parser (Scientific -> Literal)
f16 = symbol "16" $> Float16 . toRealFloat
{-# INLINE f16 #-}

f32 :: Parser (Scientific -> Literal)
f32 = symbol "32" $> Float32 . toRealFloat
{-# INLINE f32 #-}

f64 :: Parser (Scientific -> Literal)
f64 = symbol "64" $> Float64 . toRealFloat
{-# INLINE f64 #-}
