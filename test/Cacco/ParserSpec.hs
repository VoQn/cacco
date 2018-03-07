{-# LANGUAGE OverloadedStrings #-}

module Cacco.ParserSpec where

import           Test.Tasty.Hspec

import           Cacco.Syntax.Expr
import           Cacco.Syntax.Literal (Literal (..))
import           Cacco.Syntax.Parser

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_parseAst :: Spec
spec_parseAst =  do
  let testParse = parseAst "test"
  --
  it "can parse \"0xff\"" $
    testParse "0xff" `shouldBe` Right
      (Lit $ Natural 0xff)
  --
  it "can parse \"+1.0\"" $
    testParse "+1.0" `shouldBe` Right
      (Lit $ Flonum 1.0)
  --
  it "can parse \"something text\"" $
    testParse "\"something text\"" `shouldBe` Right
      (Lit $ Text "something text")
  --
  it "can parse \"true\"" $
    testParse "true" `shouldBe` Right
      (Lit $ Bool True)
  --
  it "can parse \"false\"" $
    testParse "false" `shouldBe` Right
      (Lit $ Bool False)
  --
  it "can parse \"undefined\"" $
    testParse "undefined" `shouldBe` Right
      (Lit Undef)
  --
  it "can parse \"(dec x Integer)\"" $
    testParse "(dec x Integer)" `shouldBe` Right
      (App (Sym "dec")
        [ Sym "x"
        , Sym "Integer"
        ])
  --
  it "can parse \"(= x 100)\"" $
    testParse "(= x 100)" `shouldBe` Right
      (Con (Sym "x") $ Lit $ Natural 100)
  --
  it "can parse \"(var x 100)\"" $
    testParse "(var x 100)" `shouldBe` Right
      (App (Sym "var")
        [ Sym "x"
        , Lit $ Natural 100
        ])
  --
  it "can parse \"(var x 10.0)\"" $
    testParse "(var x 10.0)" `shouldBe` Right
      (App (Sym "var")
        [ Sym "x"
        , Lit $ Flonum 10.0
        ])
  --
  it "can parse \"(set! x 0)\"" $
    testParse "(set! x 0)" `shouldBe` Right
      (App (Sym "set!")
        [ Sym "x"
        , Lit $ Natural 0
        ])
  --
  it "can parse \"(foo true false\\n undefined 2)\"" $
    testParse "(foo true false\n undefined \"hello\" 2)" `shouldBe` Right
      (App (Sym "foo")
        [ Lit $ Bool True
        , Lit $ Bool False
        , Lit Undef
        , Lit $ Text "hello"
        , Lit $ Natural 2
        ])
  --
  it "can parse \"(+ +1 -2)\"" $
    testParse "(+ +1 -2)" `shouldBe` Right
      (App (Sym "+")
        [ Lit $ Integer 1
        , Lit $ Integer (-2)
        ])
  --
  it "can parse \"(== 1 1)\"" $
    testParse "(== 1 1)" `shouldBe` Right
      (App (Sym "==")
        [ Lit $ Natural 1
        , Lit $ Natural 1
        ])
  --
  it "can parse expression with ignoreing line comments" $
    testParse ";; if this comments didn't ignore, this test case was failed.\ntrue"
      `shouldBe` Right (Lit $ Bool True)

  it "can parse expression with ignoreing block comments" $
    testParse "(; if this comments didn't ignore, this test case was failed. ;)\ntrue"
      `shouldBe` Right (Lit $ Bool True)
