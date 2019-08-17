{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.PositionSpec where

import           Control.Lens
import           Data.Text.Prettyprint.Doc      ( pretty )
import           Test.Tasty.Hspec

import           Cacco.Syntax.Position

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_as_instance_of_Pretty :: Spec
spec_as_instance_of_Pretty = do
  context "when x = Location{_sourceName = \"\", _line = 1, _column = 2}" $ do
    let x = initPosition & column .~ 2
    it "\"(unknown):1,2\"" $ (show . pretty) x `shouldBe` "((unknown):1,2)"
  context "when x = Location{_sourceName = \"test\", _line = 1, _column = 1}"
    $ do
        let x = initPosition & sourceName .~ "test" & column .~ 2
        it "\"test:1,2\"" $ (show . pretty) x `shouldBe` "(test:1,2)"

spec_as_instance_of_Ord :: Spec
spec_as_instance_of_Ord = do
  context "when a^.sourceName /= b^.sourceName" $ do
    let a = initPosition & sourceName .~ "a"
    let b = initPosition & sourceName .~ "b"
    it "return a^.sourceName `compare` b^.sourceName"
      $          (a `compare` b)
      `shouldBe` LT

  context "when a^.sourceName == b^.sourceName" $ do
    let a1 = initPosition
    let b1 = initPosition
    it "if a^.line < b.^line, return LT" $ do
      let a2 = a1 & line .~ 1
      let b2 = b1 & line .~ 2
      (a2 `compare` b2) `shouldBe` LT

    it "if a^.line > b.^line, return GT" $ do
      let a2 = a1 & line .~ 2
      let b2 = b1 & line .~ 1
      (a2 `compare` b2) `shouldBe` GT

    context "if a^.line == b.line" $ do
      it "and if a^.column == b^.column, return EQ"
        $          (a1 `compare` b1)
        `shouldBe` EQ

      it "if a^.column < b^.column, return LT" $ do
        let a2 = a1 & column .~ 1
        let b2 = b1 & column .~ 2
        (a2 `compare` b2) `shouldBe` LT

      it "and if a^.column > b^.column, return GT" $ do
        let a2 = a1 & column .~ 2
        let b2 = b1 & column .~ 1
        (a2 `compare` b2) `shouldBe` GT
