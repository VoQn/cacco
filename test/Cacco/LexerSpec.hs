{-# LANGUAGE OverloadedStrings #-}

module Cacco.LexerSpec where

import           Data.Scientific  (fromFloatDigits)
import           Test.Tasty.Hspec (Spec, context, describe, it, shouldBe)
import           Text.Megaparsec  (parse)

import qualified Cacco.Lexer      as Lexer

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Lexer :: Spec
spec_Cacco_Lexer = do
  integerParserSpec
  decimalParserSpec
  stringLiteralParserSpec

integerParserSpec :: Spec
integerParserSpec = describe "Cacco.Lexer.integer" $ do
  let parseTest = parse Lexer.integer "test"

  context "when parsing decimal integer literal" $ do
    it "can parse 0" $
      parseTest "0" `shouldBe` Right 0

    it "can parse 1" $
      parseTest "1" `shouldBe` Right 1

    it "can parse 10" $
      parseTest "10" `shouldBe` Right 10

    it "can parse 010" $
      parseTest "010" `shouldBe` Right 10

    it "can parse -1" $
      parseTest "-1" `shouldBe` Right (-1)

  context "when parsing hexadecimal integer literal" $ do
    it "can parse 0x10 as 16" $
      parseTest "0x10" `shouldBe` Right 16

    it "can parse 0x100 as 256" $
      parseTest "0x100" `shouldBe` Right 256

    it "can parse 0xa as 10" $
      parseTest "0xa" `shouldBe` Right 10

    it "can parse 0xFF as 255" $
      parseTest "0xFF" `shouldBe` Right 255

  context "when parsing octal integer literal" $ do
    it "can parse 0o10 as 8" $
      parseTest "0o10" `shouldBe` Right 8

    it "can parse 0o77 as integer" $
      parseTest "0o77" `shouldBe` Right 0o77

  context "when parsing binary integer literal" $ do
    it "can parse 0b00000000 as 0" $
      parseTest "0b00000000" `shouldBe` Right 0

    it "can parse 0b00000001 as 1" $
      parseTest "0b00000001" `shouldBe` Right 1

    it "can parse 0b00000010 as 2" $
      parseTest "0b00000010" `shouldBe` Right 2

    it "can parse 0b00000011 as 3" $
      parseTest "0b00000011" `shouldBe` Right 3

    it "can parse 0b00000100 as 4" $
      parseTest "0b00000100" `shouldBe` Right 4

    it "can parse 0b00000101 as 5" $
      parseTest "0b00000101" `shouldBe` Right 5

decimalParserSpec :: Spec
decimalParserSpec = describe "Cacco.Lexer.decimal" $ do
  let parseTest = parse Lexer.decimal "test"

  it "can parse 0.0" $
    parseTest "0.0" `shouldBe` Right (fromFloatDigits (0.0 :: Double))

  it "can parse 0.01" $
    parseTest "0.01" `shouldBe` Right (fromFloatDigits (0.01 :: Double))

  it "can parse 1.0" $
    parseTest "1.0" `shouldBe` Right (fromFloatDigits (1.0 :: Double))

  it "can parse -10.5" $
    parseTest "-10.5" `shouldBe` Right (fromFloatDigits ((-10.5) :: Double))

stringLiteralParserSpec :: Spec
stringLiteralParserSpec = describe "Cacco.Lexer.stringLiteral" $ do
  let parseTest = parse Lexer.stringLiteral "test"

  it "can parse \"\" as empty string" $
    parseTest "\"\"" `shouldBe` Right ""

  it "can parse \"\\\"\" as valid string" $
    parseTest "\"\\\"\"" `shouldBe` Right "\""

  it "can parse \"hello\" as string using ASCII characters" $
    parseTest "\"hello\"" `shouldBe` Right "hello"

  it "can parse \"こんにちは\" as string using multi-byte characters" $
    parseTest "\"こんにちは\"" `shouldBe` Right "こんにちは"
