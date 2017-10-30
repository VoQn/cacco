{-# LANGUAGE OverloadedStrings #-}

module Cacco.ParserSpec where

import           Data.Functor     ((<$>))
import           Data.Scientific  (fromFloatDigits)
import           Test.Tasty.Hspec

import qualified Cacco.Ast        as Ast
import qualified Cacco.Expr       as Expr
import           Cacco.Location
import           Cacco.Parser

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Parser :: Spec
spec_Cacco_Parser =
  parseExprSpec

parseExprSpec:: Spec
parseExprSpec = describe "Cacco.Parser.parseExpr" $ do
  let testParse = (Ast.fromExpr <$>) . parseExpr "test"

  it "can parse \"+1.0\"" $
    testParse "+1.0" `shouldBe` Right
      (Ast.Decimal $ fromFloatDigits (1.0 :: Double))

  it "can parse \"(dec x Integer)\"" $
    testParse "(dec x Integer)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "dec"
        , Ast.Symbol "x"
        , Ast.Symbol "Integer"
        ])

  it "can parse \"(val x 100)\"" $
    testParse "(val x 100)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "val"
        , Ast.Symbol "x"
        , Ast.Integer 100
        ])

  it "can parse \"(var x 100)\"" $
    testParse "(var x 100)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "var"
        , Ast.Symbol "x"
        , Ast.Integer 100
        ])

  it "can parse \"(set! x 0)\"" $
    testParse "(set! x 0)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "set!"
        , Ast.Symbol "x"
        , Ast.Integer 0
        ])

  it "can parse \"(foo true false\\n undefined 2)\"" $
    testParse "(foo true false\n undefined \"hello\" 2)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "foo"
        , Ast.Boolean True
        , Ast.Boolean False
        , Ast.Undefined
        , Ast.String  "hello"
        , Ast.Integer 2
        ])

  it "can parse \"(+ +1 -2)\"" $
    testParse "(+ +1 -2)" `shouldBe` Right
       (Ast.List
        [ Ast.Symbol "+"
        , Ast.Integer 1
        , Ast.Integer (-2)
        ])

  it "can parse expression with ignoreing line comments" $
    testParse ";; if this comments didn't ignore, this test case was failed.\ntrue"
      `shouldBe` Right (Ast.Boolean True)

  it "can parse expression with ignoreing block comments" $
    testParse ";/ if this comments didn't ignore, this test case was failed. /;\ntrue"
      `shouldBe` Right (Ast.Boolean True)
