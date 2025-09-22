{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cacco.Syntax.Parser.AST where

import Control.Comonad.Ix.IxCofree (IxCofreeF ((:<<)))
import Data.Functor.Const (Const (..))
import Data.List.NonEmpty
import Text.Megaparsec
import qualified Text.Megaparsec as MP

import Cacco.Syntax.AST
import Cacco.Syntax.Index
import Cacco.Syntax.Location
import Cacco.Syntax.Parser.Internal
import Cacco.Syntax.Parser.Lexer
import Cacco.Syntax.Parser.Literal
import Data.Functor.Ix

type IParser f a = Parser (f a)

withLocation' :: Parser (t f i) -> Parser (AnnF Location t f i)
withLocation' p = do
    (l, v) <- withLocation p
    return $ Const l :<< v

hole :: IndexProxy i -> Parser (AstF f i)
hole proxy = HoleF proxy <$ reserved "_"

dots :: IndexProxy i -> Parser (AstF f i)
dots proxy = DotsF proxy <$ reserved "..."

lit :: IndexProxy i -> Parser (AstF f i)
lit proxy = LitF proxy <$> literal

var :: IndexProxy i -> Parser (AstF f i)
var proxy = VarF proxy <$> identifier

vec :: Parser (f i) -> Parser (AstF f i)
vec p = brackets $ VecF <$> many p

ifExpr :: Parser (f Expr) -> Parser (AstF f Expr)
ifExpr p = do
    reserved "if"
    IfF <$> p <*> p <*> p
{-# INLINE ifExpr #-}

apply :: Parser (f i) -> Parser (AstF f i)
apply p = AppF <$> p <*> many p

dec :: Parser (f Patt) -> Parser (f Type) -> Parser (AstF f Expr)
dec p t = do
    reserved ":"
    DecF <$> p <*> (fromList <$> MP.some t)

def :: Parser (f Expr) -> Parser (f Patt) -> Parser (AstF f Expr)
def e p = do
    reserved "="
    DefF <$> p <*> (fromList <$> MP.some e)

type AstParser t f i =
    IParser f Expr ->
    IParser f Decl ->
    IParser f Patt ->
    IParser f Type ->
    IParser (t f) i

type IxFixAstParser t i =
    (forall f. AstParser t f Expr) ->
    (forall f. AstParser t f Decl) ->
    (forall f. AstParser t f Patt) ->
    (forall f. AstParser t f Type) ->
    IParser (IxFix t) i

-- | Parse Expression AST
exprAstF :: AstParser AstF f Expr
exprAstF e _ p t =
    lexeme $
        choice
            [ hole ExprProxy
            , try (lit ExprProxy)
            , var ExprProxy
            , vec e
            , parens $
                choice
                    [ ifExpr e
                    , try (dec p t)
                    , try (def e p)
                    , apply e
                    ]
            ]

declAstF :: AstParser AstF f Decl
declAstF _ _ _ _ =
    lexeme $
        parens $
            choice
                []

{- TODO -}

pattAstF :: AstParser AstF f Patt
pattAstF _ _ p _ =
    lexeme $
        choice
            [ hole PattProxy
            , dots PattProxy
            , try (lit PattProxy)
            , var PattProxy
            , vec p
            , parens $ apply p
            ]

typeAstF :: AstParser AstF f Type
typeAstF _ _ _ t =
    lexeme $
        choice
            [ try (lit TypeProxy)
            , var TypeProxy
            , parens $ apply t
            ]

astFix :: forall t i. IndexProxy i -> IxFixAstParser t i
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

located :: AstParser AstF f i -> AstParser (AnnF Location AstF) f i
located f e d p t = withLocation' (f e d p t)

astParser :: IndexProxy i -> Parser (Ann Location AstF i)
astParser proxy =
    astFix
        proxy
        (located exprAstF)
        (located declAstF)
        (located pattAstF)
        (located typeAstF)

exprAst :: Parser (Ann Location AstF Expr)
exprAst = astParser ExprProxy
