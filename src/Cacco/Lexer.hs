module Cacco.Lexer
  ( spaceConsumer
  , lexeme
  , symbol
  , parens
  , braces
  , angles
  , brackets
  , integer
  , decimal
  ) where

import           Data.Char                  (digitToInt)
import           Data.Functor               (void)
import           Data.List                  (foldl')
import           Data.Scientific            (Scientific)
import           Text.Megaparsec            (between, char, choice, getPosition,
                                             some, try, (<|>))
import           Text.Megaparsec.ByteString (Parser)
import           Text.Megaparsec.Char       (oneOf, spaceChar)
import qualified Text.Megaparsec.Lexer      as L

import           Cacco.Location             (Location (..), fromSourcePos)

-- | Skipping space characters and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space skipSpace skipLineComment skipBlockComment
    where
      -- |Ignore a space-characters.
      skipSpace :: Parser ()
      skipSpace = void spaceChar

      -- |Ignore single line comment.
      skipLineComment :: Parser ()
      skipLineComment = L.skipLineComment ";;"

      -- |Ignore nested block comment.
      skipBlockComment :: Parser ()
      skipBlockComment = L.skipBlockCommentNested ";/" "/;"

-- | Make parser to ignore any space and comment expressions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Make specified string to token parser
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Capture token's position range.
withLocation :: Parser a -> Parser (a, Location)
withLocation parser = do
  begin <- getPosition
  value <- parser
  end   <- getPosition
  return (value, fromSourcePos begin end)

-- | Parse a integer number.
integer :: Parser (Integer, Location)
integer = withLocation $ try positionalNotation <|> L.integer
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
    binary = char 'b' >> readBin <$> some (oneOf "01")
    -- | Convert from string binary expression to integer number.
    readBin :: String -> Integer
    readBin = fromIntegral . foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- | Parse a floating point number.
decimal :: Parser (Scientific, Location)
decimal = withLocation L.scientific
