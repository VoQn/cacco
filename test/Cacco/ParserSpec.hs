{-# LANGUAGE OverloadedStrings #-}

module Cacco.ParserSpec where

import           Test.Tasty.Hspec
import           Text.Megaparsec      (parse)

import           Cacco.Syntax.Expr
import           Cacco.Syntax.Literal (Literal (..))
import           Cacco.Syntax.Parser

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Parser :: Spec
spec_Cacco_Parser = do
  parseNumeric
  parseExprSpec

parseNumeric :: Spec
parseNumeric = describe "Cacco.Parser.numeric" $ do
  let testParse = parse numeric "<test>"
  it "can parse \"0\"" $
    testParse "0" `shouldBe` Right (Integer 0)
  --
  it "can parse \"-5\"" $
    testParse "-5" `shouldBe` Right (Integer (-5))
  --
  it "can parse \"20.5\"" $
    testParse "20.5" `shouldBe` Right (Flonum 20.5)
  --
  it "can parse \"0.0\"" $
    testParse "0.0" `shouldBe` Right (Flonum 0.0)
  --
  it "can parse \"-2.5\"" $
    testParse "-2.5" `shouldBe` Right (Flonum (-2.5))
  --

parseExprSpec:: Spec
parseExprSpec = describe "Cacco.Parser.parseExpr" $ do
  let testParse = parseAst "test"
  --
  it "can parse \"0xff\"" $
    testParse "0xff" `shouldBe` Right
      (Literal $ Integer 0xff)
  --
  it "can parse \"+1.0\"" $
    testParse "+1.0" `shouldBe` Right
      (Literal $ Flonum 1.0)
  --
  it "can parse \"something text\"" $
    testParse "\"something text\"" `shouldBe` Right
      (Literal $ Text "something text")
  --
  it "can parse \"true\"" $
    testParse "true" `shouldBe` Right
      (Literal $ Bool True)
  --
  it "can parse \"false\"" $
    testParse "false" `shouldBe` Right
      (Literal $ Bool False)
  --
  it "can parse \"undefined\"" $
    testParse "undefined" `shouldBe` Right
      (Literal Undef)
  --
  it "can parse \"(dec x Integer)\"" $
    testParse "(dec x Integer)" `shouldBe` Right
      (List
        [ Symbol "dec"
        , Symbol "x"
        , Symbol "Integer"
        ])
  --
  it "can parse \"(val x 100)\"" $
    testParse "(val x 100)" `shouldBe` Right
      (List
        [ Symbol "val"
        , Symbol "x"
        , Literal $ Integer 100
        ])
  --
  it "can parse \"(var x 100)\"" $
    testParse "(var x 100)" `shouldBe` Right
      (List
        [ Symbol "var"
        , Symbol "x"
        , Literal $ Integer 100
        ])
  --
  it "can parse \"(var x 10.0)\"" $
    testParse "(var x 10.0)" `shouldBe` Right
      (List
        [ Symbol "var"
        , Symbol "x"
        , Literal $ Flonum 10.0
        ])
  --
  it "can parse \"(set! x 0)\"" $
    testParse "(set! x 0)" `shouldBe` Right
      (List
        [ Symbol "set!"
        , Symbol "x"
        , Literal $ Integer 0
        ])
  --
  it "can parse \"(foo true false\\n undefined 2)\"" $
    testParse "(foo true false\n undefined \"hello\" 2)" `shouldBe` Right
      (List
        [ Symbol "foo"
        , Literal $ Bool True
        , Literal $ Bool False
        , Literal Undef
        , Literal $ Text "hello"
        , Literal $ Integer 2
        ])
  --
  it "can parse \"(+ +1 -2)\"" $
    testParse "(+ +1 -2)" `shouldBe` Right
      (List
        [ Symbol "+"
        , Literal $ Integer 1
        , Literal $ Integer (-2)
        ])
  --
  it "can parse expression with ignoreing line comments" $
    testParse ";; if this comments didn't ignore, this test case was failed.\ntrue"
      `shouldBe` Right (Literal $ Bool True)

  it "can parse expression with ignoreing block comments" $
    testParse "(; if this comments didn't ignore, this test case was failed. ;)\ntrue"
      `shouldBe` Right (Literal $ Bool True)
