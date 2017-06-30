{-# LANGUAGE OverloadedStrings #-}

module Cacco.ExprSpec where

-- import           Data.Scientific  (fromFloatDigits)
import           Test.Tasty.Hspec

import qualified Cacco.Expr       as Expr
import           Cacco.Location

spec_Cacco_Expr :: Spec
spec_Cacco_Expr =
  describe "instance of Show" $ do
    it "Undef" $ show (Expr.Undef (Location "test" 1 1))
      `shouldBe` "Undef (test:1,1)"

    it "True" $ show (Expr.Boolean (Location "test" 1 1) True)
      `shouldBe` "Boolean True (test:1,1)"

    it "False" $ show (Expr.Boolean (Location "test" 1 1) False)
      `shouldBe` "Boolean False (test:1,1)"

    it "1 as Integer" $ show (Expr.Integer (Location "test" 1 1) 1)
      `shouldBe` "Integer 1 (test:1,1)"

    it "1.0 as Decimal" $ show (Expr.Decimal (Location "test" 1 1) 1)
      `shouldBe` "Decimal 1.0 (test:1,1)"

    it "\"\"" $ show (Expr.String (Location "test" 1 1 ) "")
      `shouldBe` "String \"\" (test:1,1)"
