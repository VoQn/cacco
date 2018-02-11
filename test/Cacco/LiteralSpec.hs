{-# LANGUAGE OverloadedStrings #-}

module Cacco.LiteralSpec where

import           Test.Tasty.Hspec

import           Cacco.Literal    (Literal (..))

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Expr :: Spec
spec_Cacco_Expr =
  describe "instance of Show" $ do
    it "Undef" $ show Undefined
      `shouldBe` "Undefined"

    it "True" $ show (Boolean True)
      `shouldBe` "Boolean True"

    it "False" $ show (Boolean False)
      `shouldBe` "Boolean False"

    it "1 as Integer" $ show (Integer 1)
      `shouldBe` "Integer 1"

    it "1.0 as Decimal" $ show (Flonum 1)
      `shouldBe` "Flonum 1.0"

    it "\"\"" $ show (Text "")
      `shouldBe` "Text \"\""
