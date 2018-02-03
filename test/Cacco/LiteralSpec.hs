{-# LANGUAGE OverloadedStrings #-}

module Cacco.LiteralSpec where

-- import           Data.Scientific  (fromFloatDigits)
import           Test.Tasty.Hspec

import qualified Cacco.Literal        as Literal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Expr :: Spec
spec_Cacco_Expr =
  describe "instance of Show" $ do
    it "Undef" $ show Literal.Undefined
      `shouldBe` "Undefined"

    it "True" $ show (Literal.Boolean True)
      `shouldBe` "Boolean True"

    it "False" $ show (Literal.Boolean False)
      `shouldBe` "Boolean False"

    it "1 as Integer" $ show (Literal.Integer 1)
      `shouldBe` "Integer 1"

    it "1.0 as Decimal" $ show (Literal.Decimal 1)
      `shouldBe` "Decimal 1.0"

    it "\"\"" $ show (Literal.Text "")
      `shouldBe` "Text \"\""
