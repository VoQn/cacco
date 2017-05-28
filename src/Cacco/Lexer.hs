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
  ) where

import           Data.Bits             (shiftL)
import           Data.Functor          (void, ($>))
import           Data.List             (foldl')
import           Data.Scientific       (Scientific)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Megaparsec       (between, char, choice, getPosition,
                                        manyTill, some, spaceChar, try, (<|>))
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
  let location = Location.fromSourcePos begin
  return (value, location)

-- | Parse a number with sign
withSign :: Num a => Parser a -> Parser a
withSign parser = do
    f <- minus <|> plus <|> return id
    x <- parser
    return $ f x
  where
    minus :: Num a => Parser (a -> a)
    minus = char '-' $> negate
    plus :: Num a => Parser (a -> a)
    plus = char '+' $> id

-- | Parse a integer number.
integer :: Parser (Integer, Location)
integer = withLocation $ try positionalNotation <|> withSign L.integer
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
    binary = do
      _    <- char 'b'
      bits <- some $  char '0' $> False
                  <|> char '1' $> True
      return $ foldl' bitOp 0 bits
      where
        -- | Accumulate boolean as bit-shift
        bitOp :: Integer -> Bool -> Integer
        bitOp 0 False = 0
        bitOp 0 True  = 1
        bitOp v False = v `shiftL` 1
        bitOp v True  = v `shiftL` 1 + 1

-- | Parse a floating point number.
decimal :: Parser (Scientific, Location)
decimal = withLocation $ withSign L.scientific

-- | Parse a Unicode text.
stringLiteral :: Parser (Text, Location)
stringLiteral = withLocation $ do
  _   <- char '"'
  str <- manyTill L.charLiteral $ char '"'
  return $ T.pack str
