{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cacco.Syntax.Parser
  ( Parser
  , ParseError
  , parseTopLevel
  , parseExpr
  , parseAst
  , module Cacco.Syntax.Parser.Lexer
  , module Cacco.Syntax.Parser.Literal
  , module Cacco.Syntax.Parser.Numeric
  , parse
  , parseTest
  ) where

import           Data.Functor                 (Functor)
import           Data.Functor.Foldable
import           Data.Text                    (Text)
import           Text.Megaparsec              (between, choice, eof, many,
                                               parse, parseTest, try, (<|>))


import           Data.Ann                     (AnnF (..))
import qualified Data.Ann                     as Ann

import           Cacco.Syntax.Expr            (Ast, AstF (..), Expr)
import           Cacco.Syntax.Location        (Location)
import           Cacco.Syntax.Parser.Internal (ParseError, Parser)
import           Cacco.Syntax.Parser.Lexer
import           Cacco.Syntax.Parser.Literal
import           Cacco.Syntax.Parser.Numeric

contents :: Parser a -> Parser a
contents = between sc eof

fixParser :: Functor f
          => (forall a. Parser a -> Parser (f a))
          -> Parser (Fix f)
fixParser f = fmap Fix $ f $ fixParser f

addLocation :: Parser (f a) -> Parser (AnnF Location f a)
addLocation p = AnnF <$> withLocation p

defForm :: forall f. Parser f -> Parser (AstF f)
defForm p = do
  reserved "=" <|> reserved "def"
  ConF <$> p <*> p

applyForm :: Parser f -> Parser (AstF f)
applyForm p = AppF <$> p <*> many p

fuctor :: forall f. Parser f -> Parser (AstF f)
fuctor p = try (defForm p) <|> applyForm p

lit :: Parser (AstF f)
lit = LitF <$> try literal

sym :: Parser (AstF f)
sym = SymF <$> identifier

lis :: Parser f -> Parser (AstF f)
lis p = ListF <$> many p

ast :: forall f. Parser f -> Parser (AstF f)
ast p = lexeme $ choice
  [ try lit
  , sym
  , parens $ fuctor p
  , brackets $ lis p
  ]

expr :: Parser (Expr Location)
expr = fixParser $ addLocation . ast

topLevel :: Parser [Expr Location]
topLevel = many expr

type SourceName = String
type SourceCode = Text

type FontendParser a = SourceName -> SourceCode -> Either ParseError a

frontend :: Parser a -> FontendParser a
frontend p []   = frontend p "<stdin>"
frontend p name = parse (contents p) name

parseAst :: FontendParser Ast
parseAst = frontend $ (cata embed) . Ann.remove <$> expr

parseExpr :: FontendParser (Expr Location)
parseExpr = frontend expr

parseTopLevel :: FontendParser [Expr Location]
parseTopLevel = frontend topLevel
