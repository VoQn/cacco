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

-- | Indexed Functor Parser for AST_Index
type AstIxParser t f (i :: AstIx)
  =  Parser (f AstDecl) -- ^ Parser for Declaration
  -> Parser (f AstExpr) -- ^ Parser for Expression
  -> Parser (f AstPatt) -- ^ Parser for Pattern
  -> Parser (f AstType) -- ^ Parser for Typing
  -> Parser (t f i)

-- | Indexed Fix Parser for Indexed AST Functor
type AstIxFixParser t (i :: AstIx)
  =  (forall f. AstIxParser t f AstDecl) -- ^ Parser for Declaration
  -> (forall f. AstIxParser t f AstExpr) -- ^ Parser for Expression
  -> (forall f. AstIxParser t f AstPatt) -- ^ Parser for Pattern
  -> (forall f. AstIxParser t f AstType) -- ^ Parser for Typing
  -> Parser (IxFix t i)

var :: Parser Var
var = VarSym <$> identifier
{-# INLINE var #-}

hole :: Parser (AstF f AstPatt)
hole = HoleF <$ reserved "_"
{-# INLINE hole #-}

dots :: Parser (AstF f AstPatt)
dots = DotsF <$ reserved "..."
{-# INLINE dots #-}

lit :: AstIxProxy i -> Parser (AstF f i)
lit proxy = LitF <$> literal <*> pure proxy
{-# INLINE lit #-}

sym :: AstIxProxy i -> Parser (AstF f i)
sym proxy = VarF <$> var <*> pure proxy
{-# INLINE sym #-}

lis :: Parser (f i) -> Parser (AstF f i)
lis p = LisF <$> brackets (many p)
{-# INLINE lis #-}

app :: Parser (f i) -> Parser (AstF f i)
app p = AppF <$> p <*> many p
{-# INLINE app #-}

def :: Parser (f AstExpr)
    -> Parser (f AstPatt)
    -> Parser (AstF f AstDecl)
def pe pp = do
  reserved "=" <|> reserved "def"
  DefF <$> pp <*> pe
{-# INLINE def #-}

dec :: Parser (f AstType)
    -> Parser (AstF f AstDecl)
dec t = do
  reserved ":" <|> reserved "dec"
  DecF <$> var <*> many t
{-# INLINE dec #-}

ifStmt :: Parser (f AstExpr)
       -> Parser (AstF f AstExpr)
ifStmt e = do
  reserved "if"
  IfF <$> e <*> e <*> e
{-# INLINE ifStmt #-}

lambda :: Parser (f AstExpr)
       -> Parser (f AstPatt)
       -> Parser (AstF f AstExpr)
lambda e p = LamF <$> many p <*> e
{-# INLINE lambda #-}
--

-- | parse Declaration AST
declAstF :: AstIxParser AstF f AstDecl
declAstF _ e p t = lexeme $ parens $ choice [ dec t, def e p ]

-- | parse Expression AST
exprAstF :: AstIxParser AstF f AstExpr
exprAstF _ e p _ = lexeme $ choice
  [ try $ lit ExprProxy
  , sym ExprProxy
  , lis e
  , parens $ choice
    [ try $ ifStmt e
    , try $ lambda e p
    , app e
    ]
  ]

-- | parse Pattern AST
pattAstF :: AstIxParser AstF f AstPatt
pattAstF _ _ p _ = lexeme $ choice
  [ hole
  , dots
  , try $ lit PattProxy
  , sym PattProxy
  , lis p
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
    {-# INLINE d' #-}
    e' = exprFix d e p t
    {-# INLINE e' #-}
    p' = pattFix d e p t
    {-# INLINE p' #-}
    t' = typeFix d e p t
    {-# INLINE t' #-}
{-# INLINEABLE declFix #-}

exprFix :: forall t. IxFunctor t => AstIxFixParser t AstExpr
exprFix d e p t = In <$> e d' e' p' t'
  where
    d' = declFix d e p t
    {-# INLINE d' #-}
    e' = exprFix d e p t
    {-# INLINE e' #-}
    p' = pattFix d e p t
    {-# INLINE p' #-}
    t' = typeFix d e p t
    {-# INLINE t' #-}
{-# INLINEABLE exprFix #-}

pattFix :: forall t. IxFunctor t => AstIxFixParser t AstPatt
pattFix d e p t = In <$> p d' e' p' t'
  where
    d' = declFix d e p t
    {-# INLINE d' #-}
    e' = exprFix d e p t
    {-# INLINE e' #-}
    p' = pattFix d e p t
    {-# INLINE p' #-}
    t' = typeFix d e p t
    {-# INLINE t' #-}
{-# INLINEABLE pattFix #-}

typeFix :: forall t. IxFunctor t => AstIxFixParser t AstType
typeFix d e p t = In <$> t d' e' p' t'
  where
    d' = declFix d e p t
    {-# INLINE d' #-}
    e' = exprFix d e p t
    {-# INLINE e' #-}
    p' = pattFix d e p t
    {-# INLINE p' #-}
    t' = typeFix d e p t
    {-# INLINE t' #-}
{-# INLINEABLE typeFix #-}

located :: forall f (i :: AstIx). AstIxParser AstF f i -> AstIxParser (IxAnnF Location AstF) f i
located f d e p t = IxAnnF <$> withLocation (f d e p t)
{-# INLINE located #-}

declAst :: Parser (IxAnn Location AstF AstDecl)
declAst = declFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)
{-# INLINEABLE declAst #-}

exprAst :: Parser (IxAnn Location AstF AstExpr)
exprAst = exprFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)
{-# INLINEABLE exprAst #-}

pattAst :: Parser (IxAnn Location AstF AstPatt)
pattAst = pattFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)
{-# INLINEABLE pattAst #-}

typeAst :: Parser (IxAnn Location AstF AstType)
typeAst = typeFix (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)
{-# INLINEABLE typeAst #-}
