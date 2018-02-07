{-# LANGUAGE OverloadedStrings #-}

module Cacco.Parser
  ( parseTopLevel
  , parseExpr
  , numeric
  ) where

import           Cacco.Ast           (Ast)
import qualified Cacco.Ast           as Ast
import           Cacco.Lexer         (Parser, lexeme, parens, spaceConsumer)
import qualified Cacco.Lexer         as Lexer
import           Cacco.Literal       (Literal)
import qualified Cacco.Literal       as Lit
import           Cacco.Location      (Location)
import           Control.Applicative ((*>), (<*))
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Text.Megaparsec     (Token, choice, eof, many, parse, sepEndBy,
                                      try, (<?>), (<|>))
import qualified Text.Megaparsec     as Megaparsec

withLocation :: Parser (Ast Location) -> Parser (Ast Location)
withLocation p = do
  (x, l) <- Lexer.withLocation p
  return $ Ast.With l x

undef :: Parser Literal
undef = (Lexer.symbol "undefined" >> return Lit.Undefined) <?> "undefined"

bool :: Parser Literal
bool = Lit.Boolean <$> choice [true, false] <?> "boolean"
  where
    true :: Parser Bool
    true = Lexer.symbol "true" >> return True
    false :: Parser Bool
    false = Lexer.symbol "false" >> return False

integer :: Parser Literal
integer = Lit.Integer <$> Lexer.integer <?> "integer literal"

decimal :: Parser Literal
decimal = Lit.Decimal <$> Lexer.decimal <?> "decimal literal"

numeric :: Parser Literal
numeric = try decimal <|> integer

text :: Parser Literal
text = Lit.Text <$> Lexer.stringLiteral

literal :: Parser Literal
literal = undef <|> bool <|> text <|> numeric

symbol :: Parser (Ast a)
symbol = Ast.Symbol <$> Lexer.identifier

atom :: Parser (Ast Location)
atom = withLocation $ try lit <|> symbol
  where
    lit :: Parser (Ast a)
    lit = Ast.Literal <$> literal

list :: Parser (Ast Location)
list = withLocation $ Ast.List <$> parens elements
  where
    elements :: Parser [Ast Location]
    elements = form `sepEndBy` spaceConsumer

form :: Parser (Ast Location)
form = lexeme $ choice [list, atom]

contents :: Parser a -> Parser a
contents parser = spaceConsumer *> parser <* eof

topLevel :: Parser [Ast Location]
topLevel = many form

type SourceName = String
type SourceCode = Text

type ParseError = Megaparsec.ParseError (Token Text) Void

parseExpr :: SourceName -> SourceCode -> Either ParseError (Ast Location)
parseExpr []   = parseExpr "<stdin>"
parseExpr name = parse (contents form) name

parseTopLevel :: SourceName -> SourceCode -> Either ParseError [Ast Location]
parseTopLevel []   = parseTopLevel "<stdin>"
parseTopLevel name = parse (contents topLevel) name
