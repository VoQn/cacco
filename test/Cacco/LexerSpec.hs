{-# LANGUAGE OverloadedStrings #-}

module Cacco.LexerSpec where

import           Data.Int             ()
import           Data.Monoid          ((<>))
import qualified Data.Text            as Text
import           Data.Word
import           Test.Tasty.Hspec     (Spec, context, it, shouldBe)

import           Cacco.Syntax.Literal
import           Cacco.Syntax.Parser  hiding (parseTest)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Lexer_bool :: Spec
spec_Lexer_bool = do
  let parseTest = parse bool "test"
  --
  it "can parse true" $
    parseTest "true" `shouldBe` Right (Bool True)
  --
  it "can parse false" $
    parseTest "false" `shouldBe` Right (Bool False)
--
spec_Lexer_integer :: Spec
spec_Lexer_integer = do
  let parseTest = parse integer "test"

  context "when parsing decimal integer literal" $ do
    it "can parse 0" $
      parseTest "0" `shouldBe` Right (Integer 0)

    it "can parse 1" $
      parseTest "1" `shouldBe` Right (Integer 1)

    it "can parse 10" $
      parseTest "10" `shouldBe` Right (Integer 10)

    it "can parse 010" $
      parseTest "010" `shouldBe` Right (Integer 10)

    it "can parse -1" $
      parseTest "-1" `shouldBe` Right (Integer (-1))

  context "when parsing hexadecimal integer literal" $ do
    it "can parse 0x0 as 0" $
      parseTest "0x0" `shouldBe` Right (Integer 0)
    --
    it "can parse 0x1 as 1" $
      parseTest "0x1" `shouldBe` Right (Integer 1)
    --
    it "can parse 0xa as 10" $
      parseTest "0xa" `shouldBe` Right (Integer 10)
    --
    it "can parse 0x10 as 16" $
      parseTest "0x10" `shouldBe` Right (Integer 16)
    --
    it "can parse 0x11 as 17" $
      parseTest "0x11" `shouldBe` Right (Integer 17)
    --
    it "can parse 0xFF as 255" $
      parseTest "0xFF" `shouldBe` Right (Integer 255)
    --
    it "can parse 0x100 as 256" $
      parseTest "0x100" `shouldBe` Right (Integer 256)
  --
  context "when parsing octal integer literal" $ do
    it "can parse 0o10 as 8" $
      parseTest "0o10" `shouldBe` Right (Integer 8)

    it "can parse 0o77 as integer" $
      parseTest "0o77" `shouldBe` Right (Integer 0o77)

  context "when parsing binary integer literal" $ do
    it "can parse 0b00000000 as 0" $
      parseTest "0b00000000" `shouldBe` Right (Integer 0)

    it "can parse 0b00000001 as 1" $
      parseTest "0b00000001" `shouldBe` Right (Integer 1)

    it "can parse 0b00000010 as 2" $
      parseTest "0b00000010" `shouldBe` Right (Integer 2)

    it "can parse 0b00000011 as 3" $
      parseTest "0b00000011" `shouldBe` Right (Integer 3)

    it "can parse 0b00000100 as 4" $
      parseTest "0b00000100" `shouldBe` Right (Integer 4)

    it "can parse 0b00000101 as 5" $
      parseTest "0b00000101" `shouldBe` Right (Integer 5)

  context "when parsing number with 8bit int suffix" $ do
    it "can parse 0_i8" $
      parseTest "0_i8" `shouldBe` Right (Int8 0)

    it "can parse +1_i8" $
      parseTest "+1_i8" `shouldBe` Right (Int8 1)

    it "can parse -1_i8" $
      parseTest "-1_i8" `shouldBe` Right (Int8 (-1))

  context "when parsing number with 16bit int suffix" $ do
    it "can parse 0_i16" $
      parseTest "0_i16" `shouldBe` Right (Int16 0)

    it "can parse +1_i16" $
      parseTest "+1_i16" `shouldBe` Right (Int16 1)

    it "can parse -1_i16" $
      parseTest "-1_i16" `shouldBe` Right (Int16 (-1))

  context "when parsing number with 32bit int suffix" $ do
    it "can parse 0_i32" $
      parseTest "0_i32" `shouldBe` Right (Int32 0)
    --
    it "can parse +1_i32" $
      parseTest "+1_i32" `shouldBe` Right (Int32 1)
    --
    it "can parse -1_i32" $
      parseTest "-1_i32" `shouldBe` Right (Int32 (-1))
    --
  --
  context "when parsing number with 64bit int suffix" $ do
    it "can parse 0_i64" $
      parseTest "0_i64" `shouldBe` Right (Int64 0)
    --
    it "can parse +1_i64" $
      parseTest "+1_i64" `shouldBe` Right (Int64 1)
    --
    it "can parse -1_i64" $
      parseTest "-1_i64" `shouldBe` Right (Int64 (-1))
    --
  --
--

prop_Lexer_integer_parse_uint8 :: Word8 -> Bool
prop_Lexer_integer_parse_uint8 x =
  let
    parse' = parse integer "test"
    expr = Text.pack $ show x <> "_u8"
  in parse' expr == Right (Uint8 x)

prop_Lexer_integer_parse_uint16 :: Word16 -> Bool
prop_Lexer_integer_parse_uint16 x =
  let
    parse' = parse integer "test"
    expr = Text.pack $ show x <> "_u16"
  in parse' expr == Right (Uint16 x)

prop_Lexer_integer_parse_uint32 :: Word32 -> Bool
prop_Lexer_integer_parse_uint32 x =
  let
    parse' = parse integer "test"
    expr = Text.pack $ show x <> "_u32"
  in parse' expr == Right (Uint32 x)

prop_Lexer_integer_parse_uint64 :: Word64 -> Bool
prop_Lexer_integer_parse_uint64 x =
  let
    parse' = parse integer "test"
    expr = Text.pack $ show x <> "_u64"
  in parse' expr == Right (Uint64 x)

spec_Lexer_flonum :: Spec
spec_Lexer_flonum = do
  let parseTest = parse flonum "test"

  context "when parsing floating point number expression" $ do
    it "can parse 0.0" $
      parseTest "0.0" `shouldBe` Right (Flonum 0)
    --
    it "can parse 0.01" $
      parseTest "0.01" `shouldBe` Right (Flonum 0.01)
    --
    it "can parse 1.0" $
      parseTest "1.0" `shouldBe` Right (Flonum 1.0)
    --
    it "can parse -10.5" $
      parseTest "-10.5" `shouldBe` Right (Flonum (-10.5))
    --
    it "can parse 1e5" $
      parseTest "1e5" `shouldBe` Right (Flonum 1e5)
    --
    it "can parse 1e+5" $
      parseTest "1e+5" `shouldBe` Right (Flonum 1e+5)
    --
    it "can parse 1e-5" $
      parseTest "1e-5" `shouldBe` Right (Flonum 1e-5)
    --
  context "when parsing number with \"_f16\" suffix" $ do
    it "can parse 0.0_f16" $
      parseTest "0.0_f16" `shouldBe` Right (Float16 0)
    --
    it "can parse 1.0_f16" $
      parseTest "1.0_f16" `shouldBe` Right (Float16 1)
    --
    it "can parse -1.0_f16" $
      parseTest "-1.0_f16" `shouldBe` Right (Float16 (-1))
    --
    it "can parse 1.0e2_f16" $
      parseTest "1.0e2_f16" `shouldBe` Right (Float16 1e2)
    --

prop_Lexer_flonum_parse_float32 :: Float -> Bool
prop_Lexer_flonum_parse_float32 x =
  let
    parse' = parse flonum "test"
    expr = Text.pack $ show x <> "_f32"
  in parse' expr == Right (Float32 x)
--

prop_Lexer_flonum_parse_float64 :: Double -> Bool
prop_Lexer_flonum_parse_float64 x =
  let
    parse' = parse flonum "test"
    expr = Text.pack $ show x <> "_f64"
  in parse' expr == Right (Float64 x)

spec_Lexer_stringLiteral :: Spec
spec_Lexer_stringLiteral = do
  let parseTest = parse stringLiteral "test"

  it "can parse \"\" as empty string" $
    parseTest "\"\"" `shouldBe` Right ""
  --
  it "can parse \"\\\"\" as valid string" $
    parseTest "\"\\\"\"" `shouldBe` Right "\""
  --
  it "can parse \"hello\"" $
    parseTest "\"hello\"" `shouldBe` Right "hello"
  --
  it "can parse \"こんにちは\"" $
    parseTest "\"こんにちは\"" `shouldBe` Right "こんにちは"
  --
  it "can parse \"您好\"" $
    parseTest "\"您好\"" `shouldBe` Right "您好"
  --
  it "can parse \"💯\"" $
    parseTest "\"💯\"" `shouldBe` Right "💯"
  --
