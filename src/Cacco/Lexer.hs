{-# LANGUAGE OverloadedStrings #-}

module Cacco.Lexer
  ( Parser
  , spaceConsumer
  , lexeme
  , withLocation
  , symbol
  , parens, braces, angles, brackets
  , bool
  , integer
  , flonum
  , stringLiteral
  , identifier
  ) where

import           Control.Applicative        ((<*>))
import           Data.Bits                  (shiftL)
import           Data.Functor               (($>))
import           Data.List                  (foldl')
import           Data.Scientific            (Scientific, toRealFloat)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (symbolChar)
import qualified Text.Megaparsec.Char.Lexer as L

import           Cacco.Literal              (Literal)
import qualified Cacco.Literal              as Lit
import           Cacco.Location             (Location (..))
import qualified Cacco.Location             as Location

type Parser = Parsec Void Text

-- | Skipping space characters and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment

-- | Ignore single line comment.
lineComment :: Parser ()
lineComment = L.skipLineComment ";;"
{-# INLINE lineComment #-}

-- | Ignore nested block comment.
blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "(;" ";)"
{-# INLINE blockComment #-}

-- | Make parser to ignore any space and comment expressions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Make specified string to token parser
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = symbol "(" `between` symbol ")"

braces :: Parser a -> Parser a
braces = symbol "{" `between` symbol "}"

angles :: Parser a -> Parser a
angles = symbol "<" `between` symbol ">"

brackets :: Parser a -> Parser a
brackets = symbol "[" `between` symbol "]"

withLocation :: Parser a -> Parser (Location, a)
-- ^ Capture token's 'Location'.
--
-- >>> parse (withLocation integer) "test" "10"
-- (test:1,1-1,3,Integer 10)
--
withLocation parser = do
  begin <- getPosition
  value <- parser
  end   <- getPosition
  let location = Location.fromSourcePos begin end
  return (location, value)

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

bool :: Parser Literal
-- ^ Parse a boolean literal
--
-- >>> parseTest bool "true"
-- Bool True
--
-- >>> parseTest bool "false"
-- Bool False
--
bool = Lit.Bool <$> (true <|> false) <?> "boolean literal: true or false"
  where
    true :: Parser Bool
    true = symbol "true" $> True
    {-# INLINE true #-}

    false :: Parser Bool
    false = symbol "false" $> False
    {-# INLINE false #-}

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
    wrapper            <- option Lit.Integer $ suffix isSigned
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
i8  = symbol "8"  $> Lit.Int8  . fromInteger
{-# INLINE i8 #-}

i16 :: Parser (Integer -> Literal)
i16 = symbol "16" $> Lit.Int16 . fromInteger
{-# INLINE i16 #-}

i32 :: Parser (Integer -> Literal)
i32 = symbol "32" $> Lit.Int32 . fromInteger
{-# INLINE i32 #-}

i64 :: Parser (Integer -> Literal)
i64 = symbol "64" $> Lit.Int64 . fromInteger
{-# INLINE i64 #-}

u8 :: Parser (Integer -> Literal)
u8  = symbol "8"  $> Lit.Uint8  . fromInteger
{-# INLINE u8 #-}

u16 :: Parser (Integer -> Literal)
u16 = symbol "16" $> Lit.Uint16 . fromInteger
{-# INLINE u16 #-}

u32 :: Parser (Integer -> Literal)
u32 = symbol "32" $> Lit.Uint32 . fromInteger
{-# INLINE u32 #-}

u64 :: Parser (Integer -> Literal)
u64 = symbol "64" $> Lit.Uint64 . fromInteger
{-# INLINE u64 #-}

-- | Parse a floating point number.
flonum :: Parser Literal
flonum = float <?> "floating point number"
  where
    float :: Parser Literal
    float = do
      _       <- lookAhead $ try digitFloatFormat
      number  <- snd <$> withSign L.scientific
      wrapper <- option Lit.Flonum suffix
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
f16 = symbol "16" $> Lit.Float16 . toRealFloat
{-# INLINE f16 #-}

f32 :: Parser (Scientific -> Literal)
f32 = symbol "32" $> Lit.Float32 . toRealFloat
{-# INLINE f32 #-}

f64 :: Parser (Scientific -> Literal)
f64 = symbol "64" $> Lit.Float64 . toRealFloat
{-# INLINE f64 #-}

-- | Parse a Unicode text.
stringLiteral :: Parser Text
stringLiteral = T.pack <$> quoted <?> "string literal"
  where
    quoted = char '"' >> L.charLiteral `manyTill` char '"'
    {-# INLINE quoted #-}

symbolChar :: Parser Char
symbolChar = oneOf ("!@#$%^&*_+-=|:<>?/" :: String)

identifier :: Parser String
identifier = (:) <$> identInitial <*> many identTrail

identInitial :: Parser Char
identInitial = letterChar <|> symbolChar
{-# INLINE identInitial #-}

identTrail :: Parser Char
identTrail = letterChar <|> digitChar <|> symbolChar
{-# INLINE identTrail #-}
