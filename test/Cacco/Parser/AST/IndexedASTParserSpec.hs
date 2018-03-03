{-# LANGUAGE OverloadedStrings #-}
module Cacco.Parser.AST.IndexedASTParserSpec where

import           Test.Tasty.Hspec
import qualified Text.Megaparsec                 as MP

import qualified Data.IxAnn                      as IxAnn

import           Cacco.Syntax.AST.Indexed
import           Cacco.Syntax.Literal
import           Cacco.Syntax.Parser.AST.Indexed

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_declAst :: Spec
spec_declAst = do
  let declParser = fmap IxAnn.removeAnn . MP.parse declAst "spec about `declAst`"

  context "Typing declaration" $
    it "(: x Integer)" $
      declParser "(: x Integer)" `shouldBe` Right
        (Dec
          (VarSym "x")
          [Var (VarSym "Integer") TypeProxy])

  context "Definition, Assign" $ do
    it "(= x 10)" $
      declParser "(= x 10)" `shouldBe` Right
        (Def
          (Var (VarSym "x") PattProxy)
          (Lit (Integer 10) ExprProxy))

    it "(= [error result] (requiest url))" $
      declParser "(= [error result] (request url))" `shouldBe` Right
        (Def
          (Lis [ Var (VarSym "error") PattProxy
               , Var (VarSym "result") PattProxy
               ])
          (App (Var (VarSym "request") ExprProxy)
               [Var (VarSym "url") ExprProxy]))

    it "(= (const x _) x)" $
      declParser "(= (const x _) x)" `shouldBe` Right
        (Def
          (App (Var (VarSym "const") PattProxy)
               [ Var (VarSym "x") PattProxy
               , Hole
               ])
          (Var (VarSym "x") ExprProxy))
    --
    it "(= (and? false _ ... _) false)" $
      declParser "(= (and? false _ ... _) false)" `shouldBe` Right
        (Def
          (App (Var (VarSym "and?") PattProxy)
               [ Lit (Bool False) PattProxy
               , Hole
               , Dots
               , Hole
               ])
          (Lit (Bool False) ExprProxy))
--
spec_exprAst :: Spec
spec_exprAst = do
  let exprParser = fmap IxAnn.removeAnn . MP.parse exprAst "spec about `exprAst`"

  context "if statement" $
    it "(if true 0 1)" $
      exprParser "(if true 0 1)" `shouldBe` Right
        (If (Lit (Bool True) ExprProxy)
            (Lit (Integer 0) ExprProxy)
            (Lit (Integer 1) ExprProxy))
  --
  context "apply function" $ do
    it "(+ x 1)" $
      exprParser "(+ x 1)" `shouldBe` Right
        (App
          (Var (VarSym "+") ExprProxy)
          [Var (VarSym "x") ExprProxy,
           Lit (Integer 1) ExprProxy
          ])
    it "(null? [1 2 3])" $
      exprParser "(null? [1 2 3])" `shouldBe` Right
        (App
          (Var (VarSym "null?") ExprProxy)
          [Lis [ Lit (Integer 1) ExprProxy
               , Lit (Integer 2) ExprProxy
               , Lit (Integer 3) ExprProxy
               ]
          ])

