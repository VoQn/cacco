{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Cacco.Syntax.Parser.AST.Indexed where

import           Text.Megaparsec

import           Data.IxAnn
import           Data.IxFix

import           Cacco.Syntax.AST.Indexed
import           Cacco.Syntax.Location
import           Cacco.Syntax.Parser.Internal
import           Cacco.Syntax.Parser.Lexer
import           Cacco.Syntax.Parser.Literal


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
  , parens $ app p
  ]

-- | Parse Typing AST
typeAstF :: AstIxParser AstF f AstType
typeAstF _ _ _ _ = lexeme $ choice
  [ try $ lit TypeProxy
  , sym TypeProxy
  ]
--

astFix :: forall t (i :: AstIx). IxFunctor t => AstIxProxy i -> AstIxFixParser t i
astFix proxy d e p t = case proxy of
    DeclProxy -> d'
    ExprProxy -> e'
    PattProxy -> p'
    TypeProxy -> t'
  where
    astFix' :: (forall f. AstIxParser t f j) -> Parser (IxFix t j)
    astFix' f = In <$> f d' e' p' t'
    {-# INLINE astFix' #-}
    d' = astFix' d
    {-# INLINE d' #-}
    e' = astFix' e
    {-# INLINE e' #-}
    p' = astFix' p
    {-# INLINE t' #-}
    t' = astFix' t
{-# INLINEABLE astFix #-}

located :: forall f (i :: AstIx). AstIxParser AstF f i -> AstIxParser (IxAnnF Location AstF) f i
located f d e p t = IxAnnF <$> withLocation (f d e p t)
{-# INLINE located #-}

astParser :: AstIxProxy i -> Parser (IxAnn Location AstF i)
astParser proxy = astFix proxy (located declAstF) (located exprAstF) (located pattAstF) (located typeAstF)
{-# INLINEABLE astParser #-}

declAst :: Parser (IxAnn Location AstF AstDecl)
declAst = astParser DeclProxy
{-# INLINEABLE declAst #-}

exprAst :: Parser (IxAnn Location AstF AstExpr)
exprAst = astParser ExprProxy
{-# INLINEABLE exprAst #-}

pattAst :: Parser (IxAnn Location AstF AstPatt)
pattAst = astParser PattProxy
{-# INLINEABLE pattAst #-}

typeAst :: Parser (IxAnn Location AstF AstType)
typeAst = astParser TypeProxy
{-# INLINEABLE typeAst #-}
