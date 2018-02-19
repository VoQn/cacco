{-# LANGUAGE OverloadedStrings #-}

module Cacco.LiteralSpec where

import           Test.Tasty.Hspec

import           Cacco.Syntax.Literal (Literal (..))

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Expr :: Spec
spec_Cacco_Expr =
  describe "instance of Show" $ do
    it "Undef" $ show Undef
      `shouldBe` "Undef"

    it "True" $ show (Bool True)
      `shouldBe` "Bool True"

    it "False" $ show (Bool False)
      `shouldBe` "Bool False"

    it "1 as Integer" $ show (Integer 1)
      `shouldBe` "Integer 1"

    it "1.0 as Decimal" $ show (Flonum 1)
      `shouldBe` "Flonum 1.0"

    it "\"\"" $ show (Text "")
      `shouldBe` "Text \"\""
