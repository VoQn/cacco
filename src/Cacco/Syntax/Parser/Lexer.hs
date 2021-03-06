{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.Parser.Lexer
  ( sc
  , lexeme
  , withLocation
  , symbol
  , reserved
  , parens
  , braces
  , angles
  , brackets
  , true
  , false
  , stringLiteral
  , identifier
  )
where

import           Control.Applicative            ( (<*>) )
import           Data.Functor                   ( ($>) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Cacco.Syntax.Location          ( Location )
import           Cacco.Syntax.Parser.Internal   ( Parser
                                                , locationFromSourcePos
                                                )

-- | Skipping space characters and comments.
sc :: Parser ()
sc = L.space space1 lineComment blockComment
 where
    -- | Ignore single line comment.
  lineComment :: Parser ()
  lineComment = L.skipLineComment ";;"
  {-# INLINE lineComment #-}
  -- | Ignore nested block comment.
  blockComment :: Parser ()
  blockComment = L.skipBlockCommentNested "(;" ";)"
  {-# INLINE blockComment #-}
{-# INLINABLE sc #-}

-- | Make parser to ignore any space and comment expressions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
{-# INLINE lexeme #-}

-- | Make specified string to token parser
symbol :: Text -> Parser Text
symbol = L.symbol sc
{-# INLINE symbol #-}

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
  begin <- getSourcePos
  value <- parser
  end   <- getSourcePos
  let location = locationFromSourcePos begin end
  return (location, value)
{-# INLINABLE withLocation #-}
--

-- | reserved keyword
reserved :: Text -> Parser ()
reserved w = lexeme $ string w *> notFollowedBy alphaNumChar
{-# INLINABLE reserved #-}

keywords :: [String]
keywords =
  [ "if"
  , "def"
  , {- "dec", "val", "var", "set!", -}
    "undefined"
  , "true"
  , "false"
  , "="
  , ":"
  , "_"
  , "..."
  ]
{-# INLINE keywords #-}

symChar :: Parser Char
symChar = oneOf ("!@#$%^&*_+-=|:<>?/" :: String)
{-# INLINE symChar #-}

identifier :: Parser String
identifier = lexeme . try $ identifier' >>= check
 where
  identifier' :: Parser String
  identifier' = lexeme $ (:) <$> identInitial <*> many identTrail
  {-# INLINE identifier' #-}

  check :: String -> Parser String
  check x | x `elem` keywords = conflictWithReserved x
          | otherwise         = return x
  {-# INLINE check #-}

  conflictWithReserved x =
    fail $ "keyword " <> show x <> " cannot be an identifier"
  {-# INLINE conflictWithReserved #-}

  identInitial :: Parser Char
  identInitial = letterChar <|> symChar
  {-# INLINE identInitial #-}

  identTrail :: Parser Char
  identTrail = letterChar <|> digitChar <|> symChar
  {-# INLINE identTrail #-}

true :: Parser Bool
true = reserved "true" $> True
{-# INLINE true #-}

false :: Parser Bool
false = reserved "false" $> False
{-# INLINE false #-}

str :: Parser String
str = char '"' >> L.charLiteral `manyTill` char '"'
{-# INLINE str #-}

-- | Parse a Unicode text.
stringLiteral :: Parser Text
stringLiteral = T.pack <$> str <?> "string literal"
{-# INLINABLE stringLiteral #-}
