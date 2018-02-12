{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Cacco.Parser
  ( parseTopLevel
  , parseExpr
  , parseAst
  , numeric
  ) where

import           Cacco.Expr          (Annotated, Expr, ExprF, Info (..))
import qualified Cacco.Expr          as Expr
import           Cacco.Fix           (Fix (..))
import           Cacco.Literal       (Literal (..))

import           Cacco.Lexer         (Parser, brackets, lexeme, parens,
                                      spaceConsumer)
import qualified Cacco.Lexer         as Lexer

import           Cacco.Location      (Location)
import           Control.Applicative ((*>), (<*))
import           Data.Functor        (Functor)
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Text.Megaparsec     (ParsecT, Token, choice, eof, many, parse,
                                      sepEndBy, try, (<?>), (<|>))
import qualified Text.Megaparsec     as Megaparsec

contents :: Parser a -> Parser a
contents parser = spaceConsumer *> parser <* eof

fixParser :: Functor f
 => (forall a. ParsecT e s m a -> ParsecT e s m (f a))
 -> ParsecT e s m (Fix f)
fixParser f = Fix <$> f (fixParser f)

withLocation :: Parser (f a)
             -> Parser (Info Location f a)
withLocation p = do
  (x, l) <- Lexer.withLocation p
  return $ Info l x

undef :: Parser Literal
undef = Lexer.symbol "undefined" >> return Undef <?> "undefined"

bool :: Parser Literal
bool = Bool <$> choice [true, false] <?> "boolean"
  where
    true :: Parser Bool
    true = Lexer.symbol "true"
         >> return True
    false :: Parser Bool
    false = Lexer.symbol "false"
          >> return False

integer :: Parser Literal
integer = Integer <$> Lexer.integer <?> "integer literal"

decimal :: Parser Literal
decimal = Flonum <$> Lexer.decimal <?> "decimal literal"

numeric :: Parser Literal
numeric = try decimal <|> integer

text :: Parser Literal
text = Text <$> Lexer.stringLiteral

literal :: Parser Literal
literal = undef <|> bool <|> text <|> numeric

exprF :: Parser a -> Parser (ExprF a)
exprF p = lexeme $ choice
    [ Expr.LitF <$> try literal
    , Expr.SymF <$> Lexer.identifier
    , parens $ Expr.LisF <$> elements p
    , brackets $ Expr.VecF <$> elements p
    ]
  where
    elements :: Parser a -> Parser [a]
    elements = (`sepEndBy` spaceConsumer)

ast :: Parser Expr
ast = fixParser exprF

expr :: Parser (Annotated Location)
expr = fixParser $ withLocation . exprF

topLevel :: Parser [Annotated Location]
topLevel = many expr

type SourceName = String
type SourceCode = Text

type ParseError = Megaparsec.ParseError (Token Text) Void

type FontendParser a = SourceName -> SourceCode -> Either ParseError a

frontend :: Parser a -> FontendParser a
frontend p []   = frontend p "<stdin>"
frontend p name = parse (contents p) name

parseAst :: FontendParser Expr
parseAst = frontend ast

parseExpr :: FontendParser (Annotated Location)
parseExpr = frontend expr

parseTopLevel :: FontendParser [Annotated Location]
parseTopLevel = frontend topLevel
