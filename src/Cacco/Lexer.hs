module Cacco.Lexer
  ( spaceConsumer
  , lexeme
  , symbol
  , parens
  , braces
  , angles
  , brackets
  , integer
  ) where

import           Data.Char                  (digitToInt)
import           Data.Functor               (void)
import           Data.List                  (foldl')
import           Text.Megaparsec            (between, char, some, (<|>))
import           Text.Megaparsec.ByteString (Parser)
import           Text.Megaparsec.Char       (spaceChar)
import qualified Text.Megaparsec.Lexer      as L

-- |skipping space characters and comments
spaceConsumer :: Parser ()
spaceConsumer = L.space skipSpace skipLineComment skipBlockComment
  where
    skipSpace :: Parser ()
    skipSpace = void spaceChar

    skipLineComment :: Parser ()
    skipLineComment = L.skipLineComment ";;"

    skipBlockComment :: Parser ()
    skipBlockComment = L.skipBlockCommentNested ";/" "/;"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

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

-- |parsing integer literal
integer :: Parser Integer
integer = positionalNotation <|> L.integer
  where
    positionalNotation :: Parser Integer
    positionalNotation = char '0' >> (hexadecimal <|> octal <|> binary <|> return 0)

    hexadecimal :: Parser Integer
    hexadecimal = char 'x' >> L.hexadecimal

    octal :: Parser Integer
    octal = char 'o' >> L.octal

    binary :: Parser Integer
    binary = char 'b' >> do
      bs <- some (char '1' <|> char '0')
      return $  readBin bs

    readBin :: String -> Integer
    readBin = fromIntegral . foldl' (\acc x -> acc * 2 + digitToInt x) 0
