module Cacco.Parser
  ( parseTopLevel
  , parseExpr

  ) where

import           Control.Applicative  ((*>), (<*))
import           Data.Text            (Text)
import           Text.Megaparsec      (Dec, ParseError, Token, choice, eof,
                                       many, parse, sepEndBy, try)
import           Text.Megaparsec.Text (Parser)

import           Cacco.Expr
import qualified Cacco.Expr           as Expr
import           Cacco.Lexer          (lexeme, spaceConsumer)
import qualified Cacco.Lexer          as Lexer
import           Cacco.Location       ()

symbol :: Parser Expr
symbol = do
  (x, l) <- Lexer.withLocation Lexer.identifier
  return $ case x of
    "true"      -> Expr.Boolean l True
    "false"     -> Expr.Boolean l False
    "undefined" -> Expr.Undef l
    name        -> Expr.Symbol l name

atom :: Parser Expr
atom = choice [try decimal, try integer, string, symbol]
  where
    integer :: Parser Expr
    integer = do
      (x, l) <- Lexer.withLocation Lexer.integer
      return $ Expr.Integer l x
    decimal :: Parser Expr
    decimal = do
      (x, l) <- Lexer.withLocation Lexer.decimal
      return $ Expr.Decimal l x
    string :: Parser Expr
    string = do
      (x, l) <- Lexer.withLocation Lexer.stringLiteral
      return $ Expr.String l x

list :: Parser Expr
list = do
    (exprs, l) <- Lexer.withLocation $ Lexer.parens elements
    return $ Expr.List l exprs
  where
    elements :: Parser [Expr]
    elements = form `sepEndBy` spaceConsumer

form :: Parser Expr
form = lexeme $ choice [list, atom]

contents :: Parser a -> Parser a
contents parser = spaceConsumer *> parser <* eof

topLevel :: Parser [Expr]
topLevel = many form

type SourceName = String
type SourceCode = Text

parseExpr :: SourceName -> SourceCode -> Either (ParseError (Token Text) Dec) Expr
parseExpr []   = parseExpr "<stdin>"
parseExpr name = parse (contents form) name

parseTopLevel :: SourceName -> SourceCode -> Either (ParseError (Token Text) Dec) [Expr]
parseTopLevel []   = parseTopLevel "<stdin>"
parseTopLevel name = parse (contents topLevel) name
