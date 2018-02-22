{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cacco.Syntax.Parser
  ( Parser
  , ParseError
  , parseTopLevel
  , parseExpr
  , parseAst
  , numeric
  , module Cacco.Syntax.Parser.Lexer
  , module Cacco.Syntax.Parser.Numeric
  , parse
  , parseTest
  ) where

import           Data.Functor                 (Functor)
import           Data.Text                    (Text)
import           Text.Megaparsec              (ParsecT, between, choice, eof,
                                               many, parse, parseTest, some,
                                               try, (<?>), (<|>))

import           Cacco.Ann                    (AnnF (AnnF))
import qualified Cacco.Ann                    as Ann
import           Cacco.Fix                    (Fix (..))
import           Cacco.Syntax.Expr            (Ast, AstF (..), Expr)
import           Cacco.Syntax.Literal         (Literal (..))
import           Cacco.Syntax.Location        (Location)
import           Cacco.Syntax.Parser.Internal (ParseError, Parser)
import           Cacco.Syntax.Parser.Lexer
import qualified Cacco.Syntax.Parser.Lexer    as Lexer
import           Cacco.Syntax.Parser.Numeric

contents :: Parser a -> Parser a
contents parser = between sc eof parser

fixParser :: Functor f
          => (forall a. ParsecT e s m a -> ParsecT e s m (f a))
          -> ParsecT e s m (Fix f)
fixParser f = Fix <$> f (fixParser f)

addLocation :: Parser (f a) -> Parser (AnnF Location f a)
addLocation p = AnnF <$> withLocation p

undef :: Parser Literal
undef = reserved "undefined" >> return Undef <?> "undefined"

numeric :: Parser Literal
numeric = try flonum <|> integer

text :: Parser Literal
text = Text <$> Lexer.stringLiteral

literal :: Parser Literal
literal = undef <|> Lexer.bool <|> text <|> numeric

defForm :: Parser a -> Parser (AstF a)
defForm p = do
    (reserved "=" <|> reserved "def")
    form <- constForm
    return form
  where
    constForm = do
      n <- Lexer.identifier
      v <- p
      return $ ConF n v

fuctorF :: forall a. Parser a -> Parser (AstF a)
fuctorF p = try (defForm p) <|> applyForm
  where
    applyForm = do
      (f:args) <- some p
      return $ AppF f args

astF :: forall a. Parser a -> Parser (AstF a)
astF p = lexeme $ choice
    [ parens (fuctorF p)
    , VecF <$> brackets (many p)
    , LitF <$> try literal
    , SymF <$> Lexer.identifier
    ]

expr :: Parser (Expr Location)
expr = fixParser $ addLocation . astF

topLevel :: Parser [Expr Location]
topLevel = many expr

type SourceName = String
type SourceCode = Text

type FontendParser a = SourceName -> SourceCode -> Either ParseError a

frontend :: Parser a -> FontendParser a
frontend p []   = frontend p "<stdin>"
frontend p name = parse (contents p) name

parseAst :: FontendParser Ast
parseAst = frontend $ Ann.remove <$> expr

parseExpr :: FontendParser (Expr Location)
parseExpr = frontend expr

parseTopLevel :: FontendParser [Expr Location]
parseTopLevel = frontend topLevel
