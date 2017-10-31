module Cacco.Lexer
  ( spaceConsumer
  , lexeme
  , withLocation
  , symbol
  , parens
  , braces
  , angles
  , brackets
  , integer
  , decimal
  , stringLiteral
  , identifier
  ) where

import           Control.Applicative   ((<*>))
import           Data.Bits             (shiftL)
import           Data.Functor          (void, ($>))
import           Data.List             (foldl')
import           Data.Scientific       (Scientific)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Megaparsec       (between, char, choice, digitChar,
                                        getPosition, letterChar, many, manyTill,
                                        oneOf, option, some, spaceChar, try,
                                        (<?>), (<|>))
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Cacco.Location        (Location (..))
import qualified Cacco.Location        as Location

-- | Skipping space characters and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space skipSpace skipLineComment skipBlockComment
  where
    -- | Ignore a space-characters.
    skipSpace :: Parser ()
    skipSpace = void spaceChar
    -- | Ignore single line comment.
    skipLineComment :: Parser ()
    skipLineComment = L.skipLineComment ";;"
    -- | Ignore nested block comment.
    skipBlockComment :: Parser ()
    skipBlockComment = L.skipBlockCommentNested ";/" "/;"

-- | Make parser to ignore any space and comment expressions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Make specified string to token parser
symbol :: String -> Parser String
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
withLocation :: Parser a -> Parser (a, Location)
withLocation parser = do
  b <- getPosition
  v <- parser
  let l = Location.fromSourcePos b
  return (v, l)

-- | Parse a number with sign
withSign :: Num a => Parser a -> Parser a
withSign parser = sign <*> parser
  where
    sign :: Num a => Parser (a -> a)
    sign = option id (minus <|> plus)
    minus :: Num a => Parser (a -> a)
    minus = char '-' $> negate
    plus :: Num a => Parser (a -> a)
    plus = char '+' $> id

-- | Parse a integer number.
integer :: Parser Integer
integer = try positionalNotation <|> withSign L.integer <?> "integer number"
  where
    -- | Parse a integer with prefix for positional notation.
    positionalNotation :: Parser Integer
    positionalNotation = char '0' >> choice [hexadecimal, octal, binary]
    -- | Parse a hexadecimal integer with prefix 'x' (e.g. xFA901)
    hexadecimal :: Parser Integer
    hexadecimal = char 'x' >> L.hexadecimal
    -- | Parse a octal integer with prefix 'o' (e.g. o080)
    octal :: Parser Integer
    octal = char 'o' >> L.octal
    -- | Parse a binary integer with prefix 'b' (e.g. b101010)
    binary :: Parser Integer
    binary = foldl' bitOp 0 <$> (char 'b' >> some (zero <|> one))
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

-- | Parse a floating point number.
decimal :: Parser Scientific
decimal = withSign L.scientific <?> "floating point number"

-- | Parse a Unicode text.
stringLiteral :: Parser Text
stringLiteral = T.pack <$> string <?> "string literal"
  where
    string = char '"' >> L.charLiteral `manyTill` char '"'

symbolChar :: Parser Char
symbolChar = oneOf "!@#$%^&*_+-=|:<>?/"

identifier :: Parser String
identifier = (:) <$> initialChar <*> many tailChar
  where
    initialChar :: Parser Char
    initialChar = letterChar <|> symbolChar
    tailChar :: Parser Char
    tailChar = letterChar <|> digitChar <|> symbolChar
