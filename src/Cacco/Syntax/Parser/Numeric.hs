{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.Parser.Numeric
  ( numeric
  ) where

import           Data.Bits                    (shiftL)
import           Data.Char
import           Data.Functor                 (void, ($>))
import           Data.List                    (foldl')
import           Data.Scientific              (Scientific, scientific)
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
    bit :: Parser Char
    bit = char '0' <|> char '1' <?> "0 or 1"
    -- | Accumulate boolean as bit-shift
    acc :: Integer -> Char -> Integer
    acc 0  '0' = 0
    acc 0  '1' = 1
    acc !v '0' = v `shiftL` 1
    acc !v '1' = v `shiftL` 1 + 1
    acc _ _    = undefined
    {-# INLINE acc #-}
{-# INLINEABLE binary #-}

decimal :: Parser Integer
decimal = do
    ds <- withDigitSep digitChar
    return $ foldl' acc 0 ds
  where
    acc :: Integer -> Char -> Integer
    acc 0 '0'  = 0
    acc 0 c    = fromIntegral $ digitToInt c
    acc !v '0' = v * 10
    acc !v c   = v * 10 + fromIntegral (digitToInt c)
    {-# INLINE acc #-}
{-# INLINEABLE decimal #-}

octal :: Parser Integer
octal = do
    void $ char 'o'
    ds <- withDigitSep octDigitChar
    return $ foldl' acc 0 ds
  where
    acc :: Integer -> Char -> Integer
    acc 0 '0'  = 0
    acc 0  c   = fromIntegral (digitToInt c)
    acc !v '0' = v `shiftL` 3
    acc !v c   = v `shiftL` 3 + fromIntegral (digitToInt c)
    {-# INLINE acc #-}
{-# INLINEABLE octal #-}

hexadecimal :: Parser Integer
hexadecimal = do
    void $ char 'x'
    ds <- withDigitSep hexDigitChar
    return $ foldl' acc 0 ds
  where
    acc :: Integer -> Char -> Integer
    acc 0 '0'  = 0
    acc 0 c    = fromIntegral (digitToInt c)
    acc !v '0' = v `shiftL` 4
    acc !v c   = v `shiftL` 4 + fromIntegral (digitToInt c)
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
    n      <- decimal
    option (defaultWrapper s f n) $ trail s f n
  where
    defaultWrapper True f = Integer . f
    defaultWrapper ____ _ = Natural . fromInteger
    {-# INLINE defaultWrapper #-}
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

    suffixI True = char '_' >> signedInt <|> float'
    suffixI ____ = char '_' >> unsignedInt <|> signedInt <|> float'
    {-# INLINE suffixI #-}
{-# INLINEABLE decimalLiteral #-}

hexLiteral :: Parser Literal
hexLiteral = do
    n <- hexadecimal
    w <- option (Natural . fromInteger) $ char '_' >> unsignedInt <|> signedInt
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
        w <- option (Natural . fromInteger) $ char '_' >> unsignedInt <|> signedInt
        return $ w n
    {-# INLINE octalOrBinary #-}

-- | parse signed-integer-type suffix
signedInt :: Parser (Integer -> Literal)
signedInt = char 'i' >> choice
    [ symbol "8"  $> Int8
    , symbol "16" $> Int16
    , symbol "32" $> Int32
    , symbol "64" $> Int64
    ]
{-# INLINEABLE signedInt #-}

-- | parse unsigned-integer-type suffix
unsignedInt :: Parser (Integer -> Literal)
unsignedInt = char 'u' >> choice
    [ symbol "8"  $> Uint8  . fromInteger
    , symbol "16" $> Uint16 . fromInteger
    , symbol "32" $> Uint32 . fromInteger
    , symbol "64" $> Uint64 . fromInteger
    ]
{-# INLINEABLE unsignedInt #-}

float :: Parser (Scientific -> Literal)
float = char 'f' >> choice
    [ symbol "16" $> Float16
    , symbol "32" $> Float32
    , symbol "64" $> Float64
    ]
{-# INLINEABLE float #-}

float' :: Parser (Integer -> Literal)
float' = (. fromInteger) <$> float
{-# INLINEABLE float' #-}
