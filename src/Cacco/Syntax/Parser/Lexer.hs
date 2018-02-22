{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.Parser.Lexer
  ( sc
  , lexeme
  , withLocation
  , symbol
  , reserved
  , parens, braces, angles, brackets
  , bool
  , stringLiteral
  , identifier
  ) where

import           Control.Applicative          ((<*>))
import           Data.Functor                 (($>))
import           Data.Semigroup               ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer   as L

import           Cacco.Syntax.Literal         (Literal)
import qualified Cacco.Syntax.Literal         as Lit
import           Cacco.Syntax.Location        (Location (..))
import qualified Cacco.Syntax.Location        as Location
import           Cacco.Syntax.Parser.Internal (Parser)

-- | Skipping space characters and comments.
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    -- | Ignore single line comment.
    lineComment :: Parser ()
    lineComment = L.skipLineComment ";;"

    -- | Ignore nested block comment.
    blockComment :: Parser ()
    blockComment = L.skipBlockCommentNested "(;" ";)"

-- | Make parser to ignore any space and comment expressions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Make specified string to token parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

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

-- | reserved keyword
reserved :: Text -> Parser ()
reserved w = lexeme $ string w *> notFollowedBy alphaNumChar

keywords :: [String]
keywords = ["if", "def", {- "dec", "val", "var", "set!", -} "undefined", "true", "false", "=", ":"]

symChar :: Parser Char
symChar = oneOf ("!@#$%^&*_+-=|:<>?/" :: String)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p :: Parser String
    p = lexeme $ (:) <$> identInitial <*> many identTrail
    {-# INLINE p #-}

    check x
      | x `elem` keywords = fail $ "keyword " <> show x <> " cannot be an identifier"
      | otherwise = return x

    identInitial :: Parser Char
    identInitial = letterChar <|> symChar
    {-# INLINE identInitial #-}

    identTrail :: Parser Char
    identTrail = letterChar <|> digitChar <|> symChar
    {-# INLINE identTrail #-}

true :: Parser Bool
true = reserved "true" $> True

false :: Parser Bool
false = reserved "false" $> False

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
stringLiteral :: Parser Text
stringLiteral = T.pack <$> str <?> "string literal"
