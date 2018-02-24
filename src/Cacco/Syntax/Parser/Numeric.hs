{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.Parser.Numeric
  ( numeric
  ) where

import           Data.Bits                    (shiftL)
import           Data.Char
import           Data.Functor                 (void, ($>))
import           Data.List                    (foldl')
import           Data.Scientific              (Scientific, scientific,
                                               toRealFloat)
import           Text.Megaparsec              (choice, many, option, try, (<?>),
                                               (<|>))
import           Text.Megaparsec.Char         (char, digitChar, hexDigitChar,
                                               octDigitChar)

import           Cacco.Syntax.Literal
import           Cacco.Syntax.Parser.Internal (Parser)
import           Cacco.Syntax.Parser.Lexer

import           Prelude                      hiding (exponent)

signed :: Num a => Parser (Bool, a -> a)
signed = option (False, id) $ minus <|> plus
  where
    minus :: Num a => Parser (Bool, a -> a)
    minus = char '-' $> (True, negate)
    {-# INLINE minus #-}
    plus :: Num a => Parser (Bool, a -> a)
    plus = char '+' $> (True, id)
    {-# INLINE plus #-}
{-# INLINEABLE signed #-}

withDigitSep :: Parser a -> Parser [a]
withDigitSep p = (:) <$> p <*> many (p <|> (char '\'' >> p))
{-# INLINEABLE withDigitSep #-}

-- | Parse a binary integer with prefix 'b' (e.g. 101010)
binary :: Parser Integer
binary = char 'b' >> foldl' acc 0 <$> withDigitSep bit
  where
    bit :: Parser Bool
    bit = zero <|> one <?> "0 or 1"
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
    acc 0 False  = 0
    acc 0 True   = 1
    acc !v False = v `shiftL` 1
    acc !v True  = v `shiftL` 1 + 1
{-# INLINEABLE binary #-}

decimal :: Parser Integer
decimal = do
    ds <- withDigitSep digitChar
    return $ foldl' acc 0 ds
  where
    acc :: Integer -> Char -> Integer
    acc !v c = v * 10 + (fromIntegral $ digitToInt c)
    {-# INLINE acc #-}
{-# INLINEABLE decimal #-}

octal :: Parser Integer
octal = do
    void $ char 'o'
    ds <- withDigitSep octDigitChar
    return $ foldl' acc 0 ds
  where
    acc :: Integer -> Char -> Integer
    acc !v c = v * 8 + (fromIntegral $ digitToInt c)
    {-# INLINE acc #-}
{-# INLINEABLE octal #-}

hexadecimal :: Parser Integer
hexadecimal = do
    void $ char 'x'
    ds <- withDigitSep hexDigitChar
    return $ foldl' acc 0 ds
  where
    acc :: Integer -> Char -> Integer
    acc !v c = v * 16 + (fromIntegral $ digitToInt c)
    {-# INLINE acc #-}
{-# INLINEABLE hexadecimal #-}

data SP = SP !Integer {-# UNPACK #-} !Int

decimalPoint :: Integer -> Parser SP
decimalPoint c' = do
    void $ char '.'
    ds <- withDigitSep digitChar
    return $ foldl' acc (SP c' 0) ds
  where
    acc :: SP -> Char -> SP
    acc (SP a e') c =
      let a' = a * 10 + fromIntegral (digitToInt c)
      in SP a' $ e' - 1
    {-# INLINE acc #-}
{-# INLINEABLE decimalPoint #-}

exponent :: Int -> Parser Int
exponent e' = do
    void $ char 'e'
    (_, f) <- signed
    e <- decimal
    return $ fromInteger (f e) + e'
{-# INLINEABLE exponent #-}
--

decimalLiteral :: Parser Literal
decimalLiteral = do
    (s, f) <- signed
    n <- decimal
    option (Integer $ f n) $ trail s f n
  where
    trail s f n
      = decimalPoint' f n
      <|> expo f n
      <|> (($ f n) <$> suffixI s)
    {-# INLINE trail #-}

    decimalPoint' f n = do
      SP c e' <- decimalPoint n
      e       <- option e' $ exponent e'
      w       <- option Flonum suffixF
      return . w $ scientific (f c) e
    {-# INLINEABLE decimalPoint' #-}

    expo f n = do
      e <- exponent 0
      w <- option Flonum suffixF
      return . w $ scientific (f n) e
    {-# INLINE expo #-}

    suffixF = char '_' >> float
    {-# INLINE suffixF #-}

    suffixI True  = char '_' >> signedInt <|> float'
    suffixI False = char '_' >> unsignedInt <|> signedInt <|> float'
    {-# INLINE suffixI #-}
{-# INLINEABLE decimalLiteral #-}

hexLiteral :: Parser Literal
hexLiteral = do
    n <- hexadecimal
    w <- option Integer $ char '_' >> unsignedInt <|> signedInt
    -- TODO :: hexadecimal floating point syntax
    return $ w n
{-# INLINEABLE hexLiteral #-}

numeric :: Parser Literal
-- ^ Parse a number literal.
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
-- >>> parseTest numeric "1"
-- Integer 1
--
-- >>> parseTest numeric "0xFF"
-- Integer 256
--
-- >>> parseTest numeric "1_i8"
-- Int8 1
--
-- >>> parseTest numeric "1_u32"
-- Uint32 1
--
numeric = try positional <|> decimalLiteral
  where
    positional = do
      void $ char '0'
      octalOrBinary <|> hexLiteral
    {-# INLINE positional #-}

    octalOrBinary = do
      n <- octal <|> binary
      w <- option Integer $ char '_' >> unsignedInt <|> signedInt
      return $ w n
    {-# INLINE octalOrBinary #-}

-- | parse signed-integer-type suffix
signedInt :: Parser (Integer -> Literal)
signedInt = char 'i' >> choice [i8, i16, i32, i64]
{-# INLINEABLE signedInt #-}

-- | parse unsigned-integer-type suffix
unsignedInt :: Parser (Integer -> Literal)
unsignedInt = char 'u' >> choice [u8, u16, u32, u64]
{-# INLINEABLE unsignedInt #-}

float :: Parser (Scientific -> Literal)
float = char 'f' >> choice [f16, f32, f64]
{-# INLINEABLE float #-}

float' :: Parser (Integer -> Literal)
float' = (. fromInteger) <$> float
{-# INLINEABLE float' #-}

i8 :: Parser (Integer -> Literal)
i8  = symbol "8" $> Int8 . fromInteger
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
u8  = symbol "8" $> Uint8 . fromInteger
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

f16 :: Parser (Scientific -> Literal)
f16 = symbol "16" $> Float16 . toRealFloat
{-# INLINE f16 #-}

f32 :: Parser (Scientific -> Literal)
f32 = symbol "32" $> Float32 . toRealFloat
{-# INLINE f32 #-}

f64 :: Parser (Scientific -> Literal)
f64 = symbol "64" $> Float64 . toRealFloat
{-# INLINE f64 #-}
