{-# LANGUAGE OverloadedStrings #-}

module Cacco.ParserSpec where

import           Data.Scientific  (fromFloatDigits)
import           Test.Tasty.Hspec

import qualified Cacco.Expr       as Expr
import           Cacco.Location
import           Cacco.Parser

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Parser :: Spec
spec_Cacco_Parser =
  parseExprSpec

parseExprSpec:: Spec
parseExprSpec = describe "Cacco.Parser.parseExpr" $ do
  let testParse = parseExpr "test"
  it "can parse \"+1.0\"" $
    testParse "+1.0" `shouldBe` Right
      (Expr.Decimal (Location "test" 1 1) $ fromFloatDigits (1.0 :: Double))

  it "can parse \"(dec x Integer)\"" $
    testParse "(dec x Integer)" `shouldBe` Right
      (Expr.Declare (Location "test" 1 1) "x" $ Expr.Atom (Location "test" 1 8) "Integer")

  it "can parse \"(let x 100)\"" $
    testParse "(let x 100)" `shouldBe` Right
      (Expr.Let (Location "test" 1 1) "x" $ Expr.Integer (Location "test" 1 8) 100)

  it "can parse \"(var x 100)\"" $
    testParse "(var x 100)" `shouldBe` Right
      (Expr.Var (Location "test" 1 1) "x" $ Expr.Integer (Location "test" 1 8) 100)

  it "can parse \"(set! x 0)\"" $
    testParse "(set! x 0)" `shouldBe` Right
      (Expr.Reassign (Location "test" 1 1) "x" $ Expr.Integer (Location "test" 1 9) 0)

  it "can parse \"(foo true false\\n undefined 2)\"" $
    testParse "(foo true false\n undefined \"hello\" 2)"
      `shouldBe` Right
        (Expr.Call (Location "test" 1 1)
          (Expr.Atom (Location "test" 1  2) "foo")
          [
            Expr.Boolean (Location "test" 1  6) True,
            Expr.Boolean (Location "test" 1 11) False,
            Expr.Undef   (Location "test" 2  2),
            Expr.String  (Location "test" 2 12) "hello",
            Expr.Integer (Location "test" 2 20) 2
          ])

  it "can parse expression with ignoreing line comments" $
    testParse ";; if this comments didn't ignore, this test case was failed.\ntrue"
      `shouldBe` Right (Expr.Boolean (Location "test" 2 1) True)

  it "can parse expression with ignoreing block comments" $
    testParse ";/ if this comments didn't ignore, this test case was failed. /;\ntrue"
      `shouldBe` Right (Expr.Boolean (Location "test" 2 1) True)
