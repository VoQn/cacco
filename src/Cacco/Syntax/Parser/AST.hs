{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Cacco.Syntax.Parser.AST where

import           Data.Functor.Const           (Const (..))
import           Data.List.NonEmpty
import           Text.Megaparsec
import qualified Text.Megaparsec              as MP

import           Cacco.Syntax.AST
import           Cacco.Syntax.Index
import           Cacco.Syntax.Location
import           Cacco.Syntax.Parser.Internal
import           Cacco.Syntax.Parser.Lexer
import           Cacco.Syntax.Parser.Literal
import           Data.Functor.Ix

type IParser f a = Parser (f a)

rmAnn :: Ann Location (IxBase Ast) ~> Ast
rmAnn = icata alg where
    alg (_ :<< x) = iembed x

withLocation' :: IParser (f a) ~> IParser (AnnF Location f a)
withLocation' p = do
    (l, v) <- withLocation p
    return $ Const l :<< v

hole :: IndexProxy ~> IParser (AstF f)
hole proxy = HoleF proxy <$ reserved "_"

dots :: IndexProxy ~> IParser (AstF f)
dots proxy = DotsF proxy <$ reserved "..."

lit :: IndexProxy ~> IParser (AstF f)
lit proxy = LitF proxy <$> literal

var :: IndexProxy ~> IParser (AstF f)
var proxy = VarF proxy <$> identifier

vec :: IParser f ~> IParser (AstF f)
vec p = brackets $ VecF <$> many p

ifExpr :: IParser f Expr -> IParser (AstF f) Expr
ifExpr p = do
    reserved "if"
    IfF <$> p <*> p <*> p
{-# INLINE ifExpr #-}

apply :: IParser f ~> IParser (AstF f)
apply p = AppF <$> p <*> many p

dec :: IParser f Patt -> IParser f Type -> IParser (AstF f) Expr
dec p t = do
    reserved ":"
    DecF <$> p <*> (fromList <$> MP.some t)

def :: IParser f Expr -> IParser f Patt -> IParser (AstF f) Expr
def e p = do
    reserved "="
    DefF <$> p <*> (fromList <$> MP.some e)

type AstParser t f i
    =  IParser f Expr
    -> IParser f Decl
    -> IParser f Patt
    -> IParser f Type
    -> IParser (t f) i

type IxFixAstParser t i
    =  (forall f. AstParser t f Expr)
    -> (forall f. AstParser t f Decl)
    -> (forall f. AstParser t f Patt)
    -> (forall f. AstParser t f Type)
    -> IParser (IxFix t) i

-- | Parse Expression AST
exprAstF :: AstParser AstF f Expr
exprAstF e _ p t = lexeme $ choice
    [ hole ExprProxy
    , try (lit ExprProxy)
    , var ExprProxy
    , vec e
    , parens $ choice
        [ ifExpr e
        , try (dec p t)
        , try (def e p)
        , apply e
        ]
    ]

declAstF :: AstParser AstF f Decl
declAstF _ _ _ _ = lexeme $ parens $ choice
    [ {- TODO -} ]

pattAstF :: AstParser AstF f Patt
pattAstF _ _ p _ = lexeme $ choice
    [ hole PattProxy
    , dots PattProxy
    , try (lit PattProxy)
    , var PattProxy
    , vec p
    , parens $ apply p
    ]

typeAstF :: AstParser AstF f Type
typeAstF _ _ _ t = lexeme $ choice
    [ try (lit TypeProxy)
    , var TypeProxy
    , parens $ apply t
    ]

astFix :: forall t. IndexProxy ~> IxFixAstParser t
astFix proxy e d p t = case proxy of
    ExprProxy -> e'
    DeclProxy -> d'
    PattProxy -> p'
    TypeProxy -> t'
  where
    astFix' :: (forall f. AstParser t f j) -> IParser (IxFix t) j
    astFix' f = IxFix <$> f e' d' p' t'
    e' = astFix' e
    d' = astFix' d
    p' = astFix' p
    t' = astFix' t
--


located :: AstParser AstF f ~> AstParser (AnnF Location AstF) f
located f e d p t = withLocation' (f e d p t)

astParser :: IndexProxy ~> IParser (Ann Location AstF)
astParser proxy = astFix proxy
    (located exprAstF)
    (located declAstF)
    (located pattAstF)
    (located typeAstF)

exprAst :: IParser (Ann Location AstF) Expr
exprAst = astParser ExprProxy
