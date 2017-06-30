module Cacco.LocationSpec where

import           Control.Lens
import           Test.Tasty.Hspec

import           Cacco.Location

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_Cacco_Location :: Spec
spec_Cacco_Location = do
    asInstanceOfShow
    asInstanceOfOrd

asInstanceOfShow :: Spec
asInstanceOfShow = describe "an instance of Show" $ do
    context "when x = Location{_sourceName = \"\", _line = 1, _column = 2}" $ do
        let x = initLocation & column .~ 2
        it "show x returns \"(unknown):1,2\"" $ show x `shouldBe` "(unknown):1,2"
    context "when x = Location{_sourceName = \"test\", _line = 1, _column = 1}" $ do
        let x = initLocation & sourceName .~ "test" & column .~ 2
        it "show x returns \"test:1,2\"" $ show x `shouldBe` "test:1,2"

asInstanceOfOrd :: Spec
asInstanceOfOrd = describe "as instances of Ord" $ do
    context "when a^.sourceName /= b^.sourceName" $ do
        let a = initLocation & sourceName .~ "a"
        let b = initLocation & sourceName .~ "b"
        it "return a^.sourceName `compare` b^.sourceName" $
            (a `compare` b) `shouldBe` LT
    context "when a^.sourceName == b^.sourceName" $ do
        let a1 = initLocation
        let b1 = initLocation
        it "if a^.line < b.^line, return LT" $ do
            let a2 = a1 & line .~ 1
            let b2 = b1 & line .~ 2
            (a2 `compare` b2) `shouldBe` LT
        it "if a^.line > b.^line, return GT" $ do
            let a2 = a1 & line .~ 2
            let b2 = b1 & line .~ 1
            (a2 `compare` b2) `shouldBe` GT
        context "if a^.line == b.line" $ do
            it "and if a^.column == b^.column, return EQ" $
                (a1 `compare` b1) `shouldBe` EQ
            it "if a^.column < b^.column, return LT" $ do
                let a2 = a1 & column .~ 1
                let b2 = b1 & column .~ 2
                (a2 `compare` b2) `shouldBe` LT
            it "and if a^.column > b^.column, return GT" $ do
                let a2 = a1 & column .~ 2
                let b2 = b1 & column .~ 1
                (a2 `compare` b2) `shouldBe` GT
