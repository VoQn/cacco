module Cacco.Parser
  ( parseTopLevel
  , parseExpr

  ) where

import           Control.Applicative  ((*>), (<*))
import           Data.Text            (Text)
import           Text.Megaparsec      (Dec, ParseError, Token, choice,
                                       digitChar, eof, letterChar, many, oneOf,
                                       parse, sepEndBy, try, (<|>))
import           Text.Megaparsec.Text (Parser)

import           Cacco.Expr
import qualified Cacco.Expr           as Expr
import           Cacco.Lexer          (spaceConsumer)
import qualified Cacco.Lexer          as Lexer
import           Cacco.Location       ()

symbolChar :: Parser Char
symbolChar = oneOf "!@#$%^&*_+-=|:<>?/"

identFirst :: Parser Char
identFirst = letterChar <|> symbolChar

identTail :: Parser String
identTail = many (letterChar <|> digitChar <|> symbolChar)

identifier :: Parser String
identifier = do
  x <- identFirst
  xs <- identTail
  return (x:xs)

symbol :: Parser Expr
symbol = do
  (x, l) <- Lexer.withLocation identifier
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
    (exprs, l) <- Lexer.withLocation $ Lexer.parens (form `sepEndBy` spaceConsumer)
    return $ Expr.List l exprs

form :: Parser Expr
form = spaceConsumer *> choice [list, atom]

contents :: Parser a -> Parser a
contents = (spaceConsumer *>) . (<* eof)

topLevel :: Parser [Expr]
topLevel = many form

parseExpr :: String -> Text -> Either (ParseError (Token Text) Dec) Expr
parseExpr []   = parseExpr "<stdin>"
parseExpr name = parse (contents form) name

parseTopLevel :: String -> Text -> Either (ParseError (Token Text) Dec) [Expr]
parseTopLevel []   = parseTopLevel "<stdin>"
parseTopLevel name = parse (contents topLevel) name
