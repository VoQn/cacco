{-# LANGUAGE OverloadedStrings #-}

module Cacco.Parser.NumericSpec where

import           Data.Scientific      (fromFloatDigits)
import           Data.Semigroup       ((<>))
import qualified Data.Text            as Text
import           Data.Word
import           Test.Tasty.Hspec     (Spec, it, shouldBe)

import           Cacco.Syntax.Literal
import           Cacco.Syntax.Parser

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_numeric :: Spec
spec_numeric = do
  let testParse = parse numeric "<test>"
  it "can parse \"0\"" $
    testParse "0" `shouldBe` Right (Natural 0)
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
  it "can parse \"1e5\"" $
    testParse "1e5" `shouldBe` Right (Flonum 1e5)
  --
--
spec_with_digit_separater :: Spec
spec_with_digit_separater = do
  let testParse = parse numeric "<test>"

  it "1'234'567" $
    testParse "1'234'567" `shouldBe` Right (Natural 1234567)
  --
  it "1'2'3'4'5" $
    testParse "1'2'3'4'5" `shouldBe` Right (Natural 12345)
  --
  it "0b11'00" $
    testParse "0b11'00" `shouldBe` Right (Natural 12)
  --
  it "0o123'456" $
    testParse "0o123'456" `shouldBe` Right (Natural 0o123456)
  --
  it "0xfe'dc'ab'98'76" $
    testParse "0xfedcab9876" `shouldBe` Right (Natural 0xfedcab9876)
  --
--
spec_parse_strict_type_suffix :: Spec
spec_parse_strict_type_suffix = do
  let testParse = parse numeric "<test>"
  it "0b1111_u8" $
    testParse "0b1111_u8" `shouldBe` Right (Uint8 15)

  it "0o100_i8" $
    testParse "0o100_i8" `shouldBe` Right (Int8 64)

  it "0x33CCFF_u32" $
    testParse "0x33CCFF_u32" `shouldBe` Right (Uint32 0x33ccff)
  --
  it "0x33CCFF_i32" $
    testParse "0x33CCFF_i32" `shouldBe` Right (Int32 0x33ccff)

  it "0_f64" $
    testParse "0_f64" `shouldBe` Right (Float64 0)
  --
  it "-0_f64" $
    testParse "-0_f64" `shouldBe` Right (Float64 0)
  --
--
prop_parse_any_uint8 :: Word8 -> Bool
prop_parse_any_uint8 x =
  let
    expr = Text.pack $ show x <> "_u8"
  in
    parse numeric "test" expr == Right (Uint8 $ fromIntegral x)
--
prop_parse_any_uint16 :: Word16 -> Bool
prop_parse_any_uint16 x =
  let
    expr = Text.pack $ show x <> "_u16"
  in
    parse numeric "test" expr == Right (Uint16 $ fromIntegral x)
--
prop_parse_any_uint32 :: Word32 -> Bool
prop_parse_any_uint32 x =
  let
    expr = Text.pack $ show x <> "_u32"
  in
    parse numeric "test" expr == Right (Uint32 $ fromIntegral x)
--
prop_parse_any_uint64 :: Word64 -> Bool
prop_parse_any_uint64 x =
  let
    expr = Text.pack $ show x <> "_u64"
  in
    parse numeric "test" expr == Right (Uint64 $ fromIntegral x)
--
prop_parse_any_float32 :: Float -> Bool
prop_parse_any_float32 x =
  let
    expr = Text.pack $ show x <> "_f32"
  in
    parse numeric "test" expr == Right (Float32 $ fromFloatDigits x)
--

prop_parse_any_float64 :: Double -> Bool
prop_parse_any_float64 x =
  let
    expr = Text.pack $ show x <> "_f64"
  in
    parse numeric "test" expr == Right (Float64 $ fromFloatDigits x)
