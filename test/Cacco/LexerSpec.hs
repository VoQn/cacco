{-# LANGUAGE OverloadedStrings #-}

module Cacco.LexerSpec where

import           Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import           Text.Megaparsec  (parse)

import qualified Cacco.Lexer      as L

spec_integerLiteral :: Spec
spec_integerLiteral = describe "Cacco.Lexer.integer" $ do
  let parseTest = parse L.integer "test"

  describe "parsing decimal integer literal" $ do
    it "can parse 0 as integer" $
      parseTest "0" `shouldBe` Right 0

    it "can parse 1 as integer" $
      parseTest "1" `shouldBe` Right 1

    it "can parse 10 as integer" $
      parseTest "10" `shouldBe` Right 10

  describe "parsing hexadecimal integer literal" $ do
    it "can parse 0x10 as 16" $
      parseTest "0x10" `shouldBe` Right 16

    it "can parse 0x100 as 256" $
      parseTest "0x100" `shouldBe` Right 256

    it "can parse 0xFF as integer" $
      parseTest "0xFF" `shouldBe` Right 0xFF

  describe "parsing octal integer literal" $ do
    it "can parse 0o10 as 8" $
      parseTest "0o10" `shouldBe` Right 8

    it "can parse 0o77 as integer" $
      parseTest "0o77" `shouldBe` Right 0o77

  describe "parsing binary integer literal" $ do
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
