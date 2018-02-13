{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Cacco.Parser
  ( parseTopLevel
  , parseExpr
  , parseAst
  , numeric
  ) where

import           Control.Applicative ((*>), (<*))
import           Data.Functor        (Functor)
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Text.Megaparsec     (ParsecT, Token, choice, eof, many, parse,
                                      sepEndBy, try, (<?>), (<|>))
import qualified Text.Megaparsec     as Megaparsec

import           Cacco.Expr          (Annotated (..), Ast, AstF (..), Expr,
                                      Info (..))
import           Cacco.Fix           (Fix (..))
import           Cacco.Lexer         (Parser, brackets, lexeme, parens,
                                      spaceConsumer)
import qualified Cacco.Lexer         as Lexer
import           Cacco.Literal       (Literal (..))
import           Cacco.Location      (Location)

contents :: Parser a -> Parser a
contents parser = spaceConsumer *> parser <* eof

fixParser :: Functor f
          => (forall a. ParsecT e s m a -> ParsecT e s m (f a))
          -> ParsecT e s m (Fix f)
fixParser f = Fix <$> f (fixParser f)

withLocation :: Parser (f a) -> Parser (Info Location f a)
withLocation p = do
    (l, x) <- Lexer.withLocation p
    return (Info l x)

undef :: Parser Literal
undef = Lexer.symbol "undefined" >> return Undef <?> "undefined"

decimal :: Parser Literal
decimal = Flonum <$> Lexer.decimal <?> "decimal literal"

numeric :: Parser Literal
numeric = try decimal <|> Lexer.integer

text :: Parser Literal
text = Text <$> Lexer.stringLiteral

literal :: Parser Literal
literal = undef <|> Lexer.bool <|> text <|> numeric

exprF :: Parser a -> Parser (AstF a)
exprF p = lexeme $ choice
    [ LitF <$> try literal
    , SymF <$> Lexer.identifier
    , LisF <$> parens (elements p)
    , VecF <$> brackets (elements p)
    ]
  where
    elements :: Parser a -> Parser [a]
    elements = (`sepEndBy` spaceConsumer)

ast :: Parser Ast
ast = fixParser exprF

expr :: Parser (Expr Location)
expr = fixParser $ (Ann <$>) . withLocation . exprF

topLevel :: Parser [Expr Location]
topLevel = many expr

type SourceName = String
type SourceCode = Text

type ParseError = Megaparsec.ParseError (Token Text) Void

type FontendParser a = SourceName -> SourceCode -> Either ParseError a

frontend :: Parser a -> FontendParser a
frontend p []   = frontend p "<stdin>"
frontend p name = parse (contents p) name

parseAst :: FontendParser Ast
parseAst = frontend ast

parseExpr :: FontendParser (Expr Location)
parseExpr = frontend expr

parseTopLevel :: FontendParser [Expr Location]
parseTopLevel = frontend topLevel
