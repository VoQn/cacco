{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.Parser.Lexer
  ( spaceConsumer
  , lexeme
  , withLocation
  , symbol
  , parens, braces, angles, brackets
  , bool
  , stringLiteral
  , identifier
  ) where

import           Control.Applicative          ((<*>))
import           Data.Functor                 (($>))
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as TL
import           Text.Megaparsec              (between, getPosition, many,
                                               manyTill, (<?>), (<|>))
import           Text.Megaparsec.Char         (char, digitChar, letterChar,
                                               oneOf, space1)
import qualified Text.Megaparsec.Char.Lexer   as L

import           Cacco.Syntax.Literal         (Literal)
import qualified Cacco.Syntax.Literal         as Lit
import           Cacco.Syntax.Location        (Location (..))
import qualified Cacco.Syntax.Location        as Location
import           Cacco.Syntax.Parser.Internal (Parser)

-- | Skipping space characters and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment

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
--

true :: Parser Bool
true = symbol "true" $> True

false :: Parser Bool
false = symbol "false" $> False

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

str :: Parser String
str = char '"' >> L.charLiteral `manyTill` char '"'

-- | Parse a Unicode text.
stringLiteral :: Parser TL.Text
stringLiteral = TL.pack <$> str <?> "string literal"

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
