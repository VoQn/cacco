{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.LiteralSpec where

import           Data.Text.Prettyprint.Doc (pretty)
import           Test.Tasty.Hspec          (Spec, it, shouldBe)

import           Cacco.Syntax.Literal      (Literal (..))

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

spec_instance_of_Pretty :: Spec
spec_instance_of_Pretty = do
  it "Undef => undefined" $
    (show . pretty) Undef `shouldBe` "undefined"
  it "Unit => ()" $
    (show . pretty) Unit `shouldBe` "()"
  it "True => true" $
    (show . pretty) (Bool True) `shouldBe` "true"
  it "False => false" $
    (show . pretty) (Bool False) `shouldBe` "false"
  it "Int8 1 => 1_i8" $
    (show . pretty) (Int8 1) `shouldBe` "1_i8"
  it "Int16 1 => 1_i16" $
    (show . pretty) (Int16 1) `shouldBe` "1_i16"
  it "Int32 1 => 1_i32" $
    (show . pretty) (Int32 1) `shouldBe` "1_i32"
  it "Int64 1 => 1_i64" $
    (show . pretty) (Int64 1) `shouldBe` "1_i64"
  it "Uint8 1 => 1_u8" $
    (show . pretty) (Uint8 1) `shouldBe` "1_u8"
  it "Uint16 1 => 1_u16" $
    (show . pretty) (Uint16 1) `shouldBe` "1_u16"
  it "Uint32 1 => 1_u32" $
    (show . pretty) (Uint32 1) `shouldBe` "1_u32"
  it "Uint64 1 => 1_u64" $
    (show . pretty) (Uint64 1) `shouldBe` "1_u64"
  it "Integer 1 => +1" $
    (show . pretty) (Integer 1) `shouldBe` "+1"
  it "Integer 1 => -1" $
    (show . pretty) (Integer (-1)) `shouldBe` "-1"
  it "Numeric 1 => 1" $
    (show . pretty) (Numeric 1) `shouldBe` "1"
  it "Float 16 => 1.0_f16" $
    (show . pretty) (Float16 1) `shouldBe` "1.0_f16"
  it "Float 32 => 1.0_f32" $
    (show . pretty) (Float32 1) `shouldBe` "1.0_f32"
  it "Float 64 => 1.0_f64" $
    (show . pretty) (Float64 1) `shouldBe` "1.0_f64"
  it "Flonum => 1.0" $
    (show . pretty) (Flonum 1) `shouldBe` "1.0"
  it "Text \"\"" $
    (show . pretty) (Text "") `shouldBe` "\"\""
