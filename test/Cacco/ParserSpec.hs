{-# LANGUAGE OverloadedStrings #-}

module Cacco.ParserSpec where

import           Data.Functor     ((<$>))
import           Test.Tasty.Hspec

import qualified Cacco.Ast        as Ast
import qualified Cacco.Literal    as Lit
import           Cacco.Parser
import           Text.Megaparsec  (parse)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Parser :: Spec
spec_Cacco_Parser = do
  parseNumeric
  parseExprSpec

parseNumeric :: Spec
parseNumeric = describe "Cacco.Parser.numeric" $ do
  let testParse = parse numeric "<test>"
  it "can parse \"0\"" $
    testParse "0" `shouldBe` Right (Lit.Integer 0)
  --
  it "can parse \"-5\"" $
    testParse "-5" `shouldBe` Right (Lit.Integer (-5))
  --
  it "can parse \"20.5\"" $
    testParse "20.5" `shouldBe` Right (Lit.Decimal 20.5)
  --
  it "can parse \"0.0\"" $
    testParse "0.0" `shouldBe` Right (Lit.Decimal 0.0)
  --
  it "can parse \"-2.5\"" $
    testParse "-2.5" `shouldBe` Right (Lit.Decimal (-2.5))
  --

parseExprSpec:: Spec
parseExprSpec = describe "Cacco.Parser.parseExpr" $ do
  let testParse = (Ast.pureAst <$>) . parseExpr "test"
  --
  it "can parse \"0xff\"" $
    testParse "0xff" `shouldBe` Right
      (Ast.Literal $ Lit.Integer 0xff)
  --
  it "can parse \"+1.0\"" $
    testParse "+1.0" `shouldBe` Right
      (Ast.Literal $ Lit.Decimal 1.0)
  --
  it "can parse \"something text\"" $
    testParse "\"something text\"" `shouldBe` Right
      (Ast.Literal $ Lit.Text "something text")
  --
  it "can parse \"true\"" $
    testParse "true" `shouldBe` Right
      (Ast.Literal $ Lit.Boolean True)
  --
  it "can parse \"false\"" $
    testParse "false" `shouldBe` Right
      (Ast.Literal $ Lit.Boolean False)
  --
  it "can parse \"undefined\"" $
    testParse "undefined" `shouldBe` Right
      (Ast.Literal Lit.Undefined)
  --
  it "can parse \"(dec x Integer)\"" $
    testParse "(dec x Integer)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "dec"
        , Ast.Symbol "x"
        , Ast.Symbol "Integer"
        ])
  --
  it "can parse \"(val x 100)\"" $
    testParse "(val x 100)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "val"
        , Ast.Symbol "x"
        , Ast.Literal $ Lit.Integer 100
        ])

  it "can parse \"(var x 100)\"" $
    testParse "(var x 100)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "var"
        , Ast.Symbol "x"
        , Ast.Literal $ Lit.Integer 100
        ])
  --
  it "can parse \"(var x 10.0)\"" $
    testParse "(var x 10.0)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "var"
        , Ast.Symbol "x"
        , Ast.Literal $ Lit.Decimal 10.0
        ])
  --
  it "can parse \"(set! x 0)\"" $
    testParse "(set! x 0)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "set!"
        , Ast.Symbol "x"
        , Ast.Literal $ Lit.Integer 0
        ])
  --
  it "can parse \"(foo true false\\n undefined 2)\"" $
    testParse "(foo true false\n undefined \"hello\" 2)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "foo"
        , Ast.Literal $ Lit.Boolean True
        , Ast.Literal $ Lit.Boolean False
        , Ast.Literal Lit.Undefined
        , Ast.Literal $ Lit.Text "hello"
        , Ast.Literal $ Lit.Integer 2
        ])
  --
  it "can parse \"(+ +1 -2)\"" $
    testParse "(+ +1 -2)" `shouldBe` Right
      (Ast.List
        [ Ast.Symbol "+"
        , Ast.Literal $ Lit.Integer 1
        , Ast.Literal $ Lit.Integer (-2)
        ])
  --
  it "can parse expression with ignoreing line comments" $
    testParse ";; if this comments didn't ignore, this test case was failed.\ntrue"
      `shouldBe` Right (Ast.Literal $ Lit.Boolean True)

  it "can parse expression with ignoreing block comments" $
    testParse "(; if this comments didn't ignore, this test case was failed. ;)\ntrue"
      `shouldBe` Right (Ast.Literal $ Lit.Boolean True)
