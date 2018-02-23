{-# LANGUAGE OverloadedStrings #-}

module Cacco.Parser.NumericSpec where

import           Test.Tasty.Hspec            (Spec, context, describe, it,
                                              shouldBe)

import           Cacco.Syntax.Literal
import           Cacco.Syntax.Parser         hiding (parseTest)
import           Cacco.Syntax.Parser.Literal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_lexer_numeric :: Spec
spec_lexer_numeric = describe "Cacco.Parser.Numeric" $ do
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
