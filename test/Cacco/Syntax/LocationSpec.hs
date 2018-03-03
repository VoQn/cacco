{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.LocationSpec where

import           Control.Exception         (evaluate)
import           Control.Lens
import           Data.Text.Prettyprint.Doc (pretty)
import           Test.Tasty.Hspec

import           Cacco.Syntax.Location
import qualified Cacco.Syntax.Location     as L
import qualified Cacco.Syntax.Position     as P

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_instance_of_Pretty :: Spec
spec_instance_of_Pretty = do

  context "Location \"\" 1 1 1 1" $ do
    let x = initLocation
    it "((unknown):1,1-1,1)" $
      (show . pretty) x `shouldBe` "((unknown):1,1-1,1)"

  context "Location \"/home/cacco/foo.cacco\" 1 1 2 20" $ do
    let x = L.initLocation &
            L.sourceName .~ "/home/cacco/foo.cacco" &
            L.endLine .~ 2 &
            L.endColumn .~ 20
    it "(/home/cacco/foo.cacco:1,1-2,20)" $
      (show . pretty) x `shouldBe` "(/home/cacco/foo.cacco:1,1-2,20)"

spec_fromPositions :: Spec
spec_fromPositions = do
  it "(\"foo\":1,1) with (\"foo\":2,80)" $ do
    let a = P.initPosition &
            P.sourceName .~ "foo"
        b = P.initPosition &
            P.sourceName .~ "foo" &
            P.line .~ 2 &
            P.column .~ 80
        l = L.fromPositions a b
    l ^. L.sourceName `shouldBe` "foo"
    l ^. L.startLine `shouldBe` 1
    l ^. L.startColumn `shouldBe` 1
    l ^. L.endLine `shouldBe` 2
    l ^. L.endColumn `shouldBe` 80
  it "(\"foo\":1,1) with (\"bar\":1,1)" $ do
    let a = P.initPosition &
            P.sourceName .~ "foo"
        b = P.initPosition &
            P.sourceName .~ "bar" &
            P.line .~ 2 &
            P.column .~ 80
    evaluate (L.fromPositions a b) `shouldThrow` errorCall
        "Can not merge two positions from different sources"

spec_toPositions :: Spec
spec_toPositions = do

  it "((unknown):1,1-1,1)" $ do
    let x = L.initLocation
        s = P.initPosition
        e = P.initPosition
    toPositions x `shouldBe` (s, e)

  it "(foo.cacco:1,1-2,20)" $ do
    let src = "foo.cacco"
        x = L.initLocation &
            L.sourceName .~ src &
            L.endLine .~ 2 &
            L.endColumn .~ 80
        s = P.initPosition &
            P.sourceName .~ src
        e = P.initPosition &
            P.sourceName .~ src &
            P.line .~ 2 &
            P.column .~ 80
    toPositions x `shouldBe` (s, e)
