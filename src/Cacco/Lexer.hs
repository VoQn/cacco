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
  , decimal
  , stringLiteral
  , identifier
  ) where

import           Cacco.Literal              (Literal)
import qualified Cacco.Literal              as Lit
import           Cacco.Location             (Location (..))
import qualified Cacco.Location             as Location
import           Control.Applicative        ((<*>))
import           Data.Bits                  (shiftL)
import           Data.Functor               (($>))
import           Data.List                  (foldl')
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (symbolChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Skipping space characters and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    -- | Ignore single line comment.
    lineComment :: Parser ()
    lineComment = L.skipLineComment ";;"
    -- | Ignore nested block comment.
    blockComment :: Parser ()
    blockComment = L.skipBlockCommentNested "(;" ";)"

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

-- | Capture token's position range.
withLocation :: Parser a -> Parser (Location, a)
withLocation parser = do
  b <- getPosition
  v <- parser
  e <- getPosition
  let l = Location.fromSourcePos b e
  return (l, v)

-- | Parse a number with optional sign
withSign :: Num a => Parser a -> Parser (Bool, a)
withSign p = do
    (signed, fn) <- option (False, id) $ minus <|> plus
    number       <- p
    return (signed, fn number)
  where
    minus :: Num a => Parser (Bool, a -> a)
    minus = char '-' $> (True, negate)
    plus :: Num a => Parser (Bool, a -> a)
    plus = char '+' $> (True, id)

-- | Parse a boolean literal
bool :: Parser Literal
bool = Lit.Bool <$> (true <|> false) <?> "boolean literal: true or false"
  where
    true :: Parser Bool
    true = symbol "true" $> True
    false :: Parser Bool
    false = symbol "false" $> False
--

-- | Parse a binary integer with prefix 'b' (e.g. 101010)
binary' :: Parser Integer
binary' = foldl' bitOp 0 <$> some (zero <|> one)
  where
    -- | Parse '0' as a boolean
    zero :: Parser Bool
    zero = char '0' $> False
    -- | Parse '1' as a boolean
    one :: Parser Bool
    one = char '1' $> True
    -- | Accumulate boolean as bit-shift
    bitOp :: Integer -> Bool -> Integer
    bitOp 0 False = 0
    bitOp 0 True  = 1
    bitOp v False = v `shiftL` 1
    bitOp v True  = v `shiftL` 1 + 1

-- | Parse a integer with prefix for positional notation.
positional :: Parser Integer
positional = char '0' >> choice [hexadecimal, octal, binary]
  where
    -- | Parse a hexadecimal integer with prefix 'x' (e.g. xFA901)
    hexadecimal :: Parser Integer
    hexadecimal = char 'x' >> L.hexadecimal
    -- | Parse a octal integer with prefix 'o' (e.g. o080)
    octal :: Parser Integer
    octal = char 'o' >> L.octal
    -- | Parse a binary integer with prefix 'b' (e.g. b101010)
    binary :: Parser Integer
    binary = char 'b' >> binary'

-- | Parse a integer number with optional signed.
integer :: Parser Literal
integer = integer' <?> "integer literal"
  where
    integer' :: Parser Literal
    integer' = do
      (signed, num) <- (try notated <|> withSign L.decimal)
      wrapper       <- option Lit.Integer $ suffix signed
      return $ wrapper num

    notated :: Parser (Bool, Integer)
    notated = (,) False <$> positional

    suffix :: Bool -> Parser (Integer -> Literal)
    suffix signed
      | signed    = char '_' >> signedInt
      | otherwise = char '_' >> (signedInt <|> unsignedInt)

    signedInt = char 'i' >> choice [i8, i16, i32, i64]
    unsignedInt = char 'u' >> choice [u8, u16, u32, u64]

    i8, i16, i32, i64 :: Parser (Integer -> Literal)
    i8  = symbol "8"  $> (Lit.Int8  . fromInteger)
    i16 = symbol "16" $> (Lit.Int16 . fromInteger)
    i32 = symbol "32" $> (Lit.Int32 . fromInteger)
    i64 = symbol "64" $> (Lit.Int64 . fromInteger)

    u8, u16, u32, u64 :: Parser (Integer -> Literal)
    u8  = symbol "8"  $> (Lit.Uint8  . fromInteger)
    u16 = symbol "16" $> (Lit.Uint16 . fromInteger)
    u32 = symbol "32" $> (Lit.Uint32 . fromInteger)
    u64 = symbol "64" $> (Lit.Uint64 . fromInteger)

-- | Parse a floating point number.
decimal :: Parser Scientific
decimal = snd <$> withSign float <?> "floating point number"
  where
    float :: Parser Scientific
    float = do
      _ <- lookAhead digitFloat
      f <- L.scientific
      return f
    digitFloat = try $ some digitChar >> (char '.' <|> char 'e')


-- | Parse a Unicode text.
stringLiteral :: Parser Text
stringLiteral = T.pack <$> quoted <?> "string literal"
  where
    quoted = char '"' >> L.charLiteral `manyTill` char '"'

symbolChar :: Parser Char
symbolChar = oneOf ("!@#$%^&*_+-=|:<>?/" :: String)

identifier :: Parser String
identifier = (:) <$> initialChar <*> many tailChar
  where
    initialChar :: Parser Char
    initialChar = letterChar <|> symbolChar
    tailChar :: Parser Char
    tailChar = letterChar <|> digitChar <|> symbolChar

