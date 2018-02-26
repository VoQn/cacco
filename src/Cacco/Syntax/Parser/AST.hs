{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Cacco.Syntax.Parser.AST where

import           Text.Megaparsec

import           Data.IxFix                   ()

import           Cacco.Syntax.Ast
import           Cacco.Syntax.Parser.Internal
import           Cacco.Syntax.Parser.Lexer
import           Cacco.Syntax.Parser.Literal

var :: Parser Var
var = VarSym <$> identifier

hole :: Parser (Ast AstPatt)
hole = Hol <$ reserved "_"

dots :: Parser (Ast AstPatt)
dots = Dots <$ reserved "..."

lit :: AstIxProxy i -> Parser (Ast i)
lit proxy = Lit <$> literal <*> pure proxy

sym :: AstIxProxy i -> Parser (Ast i)
sym proxy = Var <$> var <*> pure proxy

lis :: Parser (Ast i) -> Parser (Ast i)
lis p = Lis <$> brackets (many p)

app :: Parser (Ast i) -> Parser (Ast i)
app p = App <$> p <*> many p

def :: Parser (Ast AstDecl)
def = do
  reserved "=" <|> reserved "def"
  Def <$> pattern <*> expression

dec :: Parser (Ast AstDecl)
dec = do
  reserved ":" <|> reserved "dec"
  Dec <$> var <*> many typing

ifStmt :: Parser (Ast AstExpr)
ifStmt = do
  reserved "if"
  If <$> expression <*> expression <*> expression

lambda :: Parser (Ast AstExpr)
lambda = Lam <$> many pattern <*> expression

-- | parse Declaration AST
declaration :: Parser (Ast AstDecl)
declaration = lexeme $ parens $ choice [ dec, def ]

-- | parse Expression AST
expression :: Parser (Ast AstExpr)
expression = lexeme $ choice
  [ try $ lit ExprProxy
  , sym ExprProxy
  , lis expression
  , parens $ choice
    [ try $ ifStmt
    , try $ lambda
    , app expression
    ]
  ]

-- | parse Pattern AST
pattern :: Parser (Ast AstPatt)
pattern = lexeme $ choice
  [ hole
  , dots
  , try $ lit PattProxy
  , sym PattProxy
  , lis pattern
  ]

-- | Parse Typing AST
typing :: Parser (Ast AstType)
typing = lexeme $ choice
  [ try $ lit TypeProxy
  , sym TypeProxy
  ]
