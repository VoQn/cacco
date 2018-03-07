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
        (Dec "x"
          [Var "Integer" TypeProxy])

  context "Definition, Assign" $ do
    it "(= x 10)" $
      declParser "(= x 10)" `shouldBe` Right
        (Def
          (Var "x" PattProxy)
          (Lit (Natural 10) ExprProxy))

    it "(= [error result] (requiest url))" $
      declParser "(= [error result] (request url))" `shouldBe` Right
        (Def
          (Lis [ Var "error" PattProxy
               , Var "result" PattProxy
               ])
          (App (Var "request" ExprProxy)
               [Var "url" ExprProxy]))

    it "(= (const x _) x)" $
      declParser "(= (const x _) x)" `shouldBe` Right
        (Def
          (App (Var "const" PattProxy)
               [ Var "x" PattProxy
               , Hole
               ])
          (Var "x" ExprProxy))
    --
    it "(= (and? false _ ... _) false)" $
      declParser "(= (and? false _ ... _) false)" `shouldBe` Right
        (Def
          (App (Var "and?" PattProxy)
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
            (Lit (Natural 0) ExprProxy)
            (Lit (Natural 1) ExprProxy))
  --
  context "apply function" $ do
    it "(+ x 1)" $
      exprParser "(+ x 1)" `shouldBe` Right
        (App
          (Var "+" ExprProxy)
          [Var "x" ExprProxy,
           Lit (Natural 1) ExprProxy
          ])
    it "(null? [1 2 3])" $
      exprParser "(null? [1 2 3])" `shouldBe` Right
        (App
          (Var "null?" ExprProxy)
          [Lis [ Lit (Natural 1) ExprProxy
               , Lit (Natural 2) ExprProxy
               , Lit (Natural 3) ExprProxy
               ]
          ])

