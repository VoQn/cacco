{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Cacco.Syntax.Parser.AST where

import           Text.Megaparsec

import           Data.IxAnn
import           Data.IxFix

import           Cacco.Syntax.AST
import           Cacco.Syntax.Location
import           Cacco.Syntax.Parser.Internal
import           Cacco.Syntax.Parser.Lexer
import           Cacco.Syntax.Parser.Literal

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
--
type AstIxParser t f (i :: AstIx)
  =  Parser (f AstDecl)
  -> Parser (f AstExpr)
  -> Parser (f AstPatt)
  -> Parser (f AstType)
  -> Parser (t f i)
--
type AstIxFixParser t (i :: AstIx)
  =  (forall f. AstIxParser t f AstDecl)
  -> (forall f. AstIxParser t f AstExpr)
  -> (forall f. AstIxParser t f AstPatt)
  -> (forall f. AstIxParser t f AstType)
  -> Parser (IxFix t i)

var :: Parser Var
var = VarSym <$> identifier

hole :: Parser (AstF f AstPatt)
hole = HoleF <$ reserved "_"

dots :: Parser (AstF f AstPatt)
dots = DotsF <$ reserved "..."

lit :: AstIxProxy i -> Parser (AstF f i)
lit proxy = LitF <$> literal <*> pure proxy

sym :: AstIxProxy i -> Parser (AstF f i)
sym proxy = VarF <$> var <*> pure proxy

lis :: Parser (f i) -> Parser (AstF f i)
lis p = LisF <$> brackets (many p)

app :: Parser (f i) -> Parser (AstF f i)
app p = AppF <$> p <*> many p

def :: Parser (f AstExpr)
    -> Parser (f AstPatt)
    -> Parser (AstF f AstDecl)
def pe pp = do
  reserved "=" <|> reserved "def"
  DefF <$> pp <*> pe

dec :: Parser (f AstType)
    -> Parser (AstF f AstDecl)
dec pt = do
  reserved ":" <|> reserved "dec"
  DecF <$> var <*> many pt

ifStmt :: Parser (f AstExpr)
       -> Parser (AstF f AstExpr)
ifStmt pe = do
  reserved "if"
  IfF <$> pe <*> pe <*> pe

lambda :: Parser (f AstExpr)
       -> Parser (f AstPatt)
       -> Parser (AstF f AstExpr)
lambda pe pp = LamF <$> many pp <*> pe
--

-- | parse Declaration AST
declAstF :: AstIxParser AstF f AstDecl
declAstF _ pe pp pt = lexeme $ parens $ choice [ dec pt, def pe pp ]

-- | parse Expression AST
exprAstF :: AstIxParser AstF f AstExpr
exprAstF _ pe pp _ = lexeme $ choice
  [ try $ lit ExprProxy
  , sym ExprProxy
  , lis pe
  , parens $ choice
    [ try $ ifStmt pe
    , try $ lambda pe pp
    , app pe
    ]
  ]

-- | parse Pattern AST
pattAstF :: AstIxParser AstF f AstPatt
pattAstF _ _ pp _ = lexeme $ choice
  [ hole
  , dots
  , try $ lit PattProxy
  , sym PattProxy
  , lis pp
  ]

-- | Parse Typing AST
typeAstF :: AstIxParser AstF f AstType
typeAstF _ _ _ _ = lexeme $ choice
  [ try $ lit TypeProxy
  , sym TypeProxy
  ]
--

declFix :: forall t. IxFunctor t => AstIxFixParser t AstDecl
declFix d e p t = In <$> d d' e' p' t'
  where
    d' = declFix d e p t
    e' = exprFix d e p t
    p' = pattFix d e p t
    t' = typeFix d e p t

exprFix :: forall t. IxFunctor t => AstIxFixParser t AstExpr
exprFix d e p t = In <$> e d' e' p' t'
  where
    d' = declFix d e p t
    e' = exprFix d e p t
    p' = pattFix d e p t
    t' = typeFix d e p t

pattFix :: forall t. IxFunctor t => AstIxFixParser t AstPatt
pattFix d e p t = In <$> p d' e' p' t'
  where
    d' = declFix d e p t
    e' = exprFix d e p t
    p' = pattFix d e p t
    t' = typeFix d e p t

typeFix :: forall t. IxFunctor t => AstIxFixParser t AstType
typeFix d e p t = In <$> t d' e' p' t'
  where
    d' = declFix d e p t
    e' = exprFix d e p t
    p' = pattFix d e p t
    t' = typeFix d e p t
--

located :: forall f (i :: AstIx). AstIxParser AstF f i -> AstIxParser (IxAnnF Location AstF) f i
located f d e p t = IxAnnF <$> withLocation (f d e p t)
{-# INLINE located #-}

declAst :: Parser (IxAnn Location AstF AstDecl)
declAst = declFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)

exprAst :: Parser (IxAnn Location AstF AstExpr)
exprAst = exprFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)

pattAst :: Parser (IxAnn Location AstF AstPatt)
pattAst = pattFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)

typeAst :: Parser (IxAnn Location AstF AstType)
typeAst = typeFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)
