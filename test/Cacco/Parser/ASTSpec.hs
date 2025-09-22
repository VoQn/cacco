{-# LANGUAGE OverloadedStrings #-}

module Cacco.Parser.ASTSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import qualified Text.Megaparsec as MP

import Cacco.Syntax.AST
import Cacco.Syntax.Index
import Cacco.Syntax.Literal
import Cacco.Syntax.Parser.AST
import Data.Functor.Ix.IxFree

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_declAst :: Spec
spec_declAst = do
    let declParser = fmap rmAnn . MP.parse exprAst "spec about `declAst`"

    context "Typing declaration" $
        it "(: x Integer)" $
            declParser "(: x Integer)"
                `shouldBe` Right (Dec (Var PattProxy "x") (Var TypeProxy "Integer" :| []))

    context "Definition, Assign" $ do
        it "(= x 10)" $
            declParser "(= x 10)"
                `shouldBe` Right
                    (Def (Var PattProxy "x") (Lit ExprProxy (Natural 10) :| []))

        it "(= [error result] (requiest url))" $
            declParser "(= [error result] (request url))"
                `shouldBe` Right
                    ( Def
                        (Vec [Var PattProxy "error", Var PattProxy "result"])
                        (App (Var ExprProxy "request") [Var ExprProxy "url"] :| [])
                    )

        it "(= (const x _) x)" $
            declParser "(= (const x _) x)"
                `shouldBe` Right
                    ( Def
                        (App (Var PattProxy "const") [Var PattProxy "x", Hole PattProxy])
                        (Var ExprProxy "x" :| [])
                    )
        --
        it "(= (and? false _ ... _) false)" $
            declParser "(= (and? false _ ... _) false)"
                `shouldBe` Right
                    ( Def
                        ( App
                            (Var PattProxy "and?")
                            [ Lit PattProxy $ Bool False
                            , Hole PattProxy
                            , Dots PattProxy
                            , Hole PattProxy
                            ]
                        )
                        (Lit ExprProxy (Bool False) :| [])
                    )

--
spec_exprAst :: Spec
spec_exprAst = do
    let exprParser = fmap rmAnn . MP.parse exprAst "spec about `exprAst`"

    context "if statement" $
        it "(if true 0 1)" $
            exprParser "(if true 0 1)"
                `shouldBe` Right
                    ( If
                        (Lit ExprProxy $ Bool True)
                        (Lit ExprProxy $ Natural 0)
                        (Lit ExprProxy $ Natural 1)
                    )
    --
    context "apply function" $ do
        it "(+ x 1)" $
            exprParser "(+ x 1)"
                `shouldBe` Right
                    (App (Var ExprProxy "+") [Var ExprProxy "x", Lit ExprProxy $ Natural 1])
        it "(null? [1 2 3])" $
            exprParser "(null? [1 2 3])"
                `shouldBe` Right
                    ( App
                        (Var ExprProxy "null?")
                        [ Vec
                            [ Lit ExprProxy $ Natural 1
                            , Lit ExprProxy $ Natural 2
                            , Lit ExprProxy $ Natural 3
                            ]
                        ]
                    )
