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
import           Text.Megaparsec            (SourcePos, between, char, choice,
                                             getPosition, some, try, (<|>))
import           Text.Megaparsec.ByteString (Parser)
import           Text.Megaparsec.Char       (oneOf, spaceChar)
import qualified Text.Megaparsec.Lexer      as L

-- |Skipping space characters and comments
spaceConsumer :: Parser ()
spaceConsumer = L.space skipSpace skipLineComment skipBlockComment
  where
    skipSpace :: Parser ()
    skipSpace = void spaceChar

    skipLineComment :: Parser ()
    skipLineComment = L.skipLineComment ";;"

    skipBlockComment :: Parser ()
    skipBlockComment = L.skipBlockCommentNested ";/" "/;"

-- |Make parser to ignore any space and comment expressions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- |Make specified string to token parser
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

withPositionRange :: Parser a -> Parser (a, (SourcePos, SourcePos))
withPositionRange parser = do
  begin <- getPosition
  value <- parser
  end   <- getPosition
  return (value, (begin, end))

hexadecimal :: Parser Integer
hexadecimal = char 'x' >> L.hexadecimal

octal :: Parser Integer
octal = char 'o' >> L.octal

binary :: Parser Integer
binary = char 'b' >> readBin <$> some (oneOf "01")
  where
    readBin :: String -> Integer
    readBin = fromIntegral . foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- |Parse a integer number.
integer :: Parser (Integer, (SourcePos, SourcePos))
integer = withPositionRange $ try positionalNotation <|> L.integer
  where
    positionalNotation :: Parser Integer
    positionalNotation = char '0' >> choice [hexadecimal, octal, binary]

-- |Parse a floating point number.
decimal :: Parser (Scientific, (SourcePos, SourcePos))
decimal = withPositionRange L.scientific
