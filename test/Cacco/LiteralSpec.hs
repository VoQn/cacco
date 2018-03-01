{-# LANGUAGE OverloadedStrings #-}

module Cacco.LiteralSpec where

import           Test.Tasty.Hspec

import           Cacco.Syntax.Literal (Literal (..))

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_instance_of_Eq :: Spec
spec_instance_of_Eq = do
  it "Undef == Undef" $
    Undef == Undef `shouldBe` True
  it "True == True" $
    Bool True == Bool True `shouldBe` True
  it "False == False" $
    Bool False == Bool False `shouldBe` True
  it "True != False" $
    Bool True == Bool False `shouldBe` False

prop_Integer_equality :: Integer -> Bool
prop_Integer_equality x = Integer x == Integer x

spec_instance_of_Show :: Spec
spec_instance_of_Show = do
  it "Undef" $
    show Undef `shouldBe` "Undef"
  it "True" $
    show (Bool True) `shouldBe` "Bool True"
  it "False" $
    show (Bool False) `shouldBe` "Bool False"
  it "Int8 1" $
    show (Int8 1) `shouldBe` "Int8 1"
  it "Int16 1" $
    show (Int16 1) `shouldBe` "Int16 1"
  it "Integer 1" $
    show (Integer 1) `shouldBe` "Integer 1"
  it "Flonum 1" $
    show (Flonum 1) `shouldBe` "Flonum 1.0"
  it "\"\"" $
    show (Text "") `shouldBe` "Text \"\""
