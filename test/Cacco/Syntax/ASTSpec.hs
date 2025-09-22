{-# LANGUAGE OverloadedStrings #-}

module Cacco.Syntax.ASTSpec where

import Prettyprinter (pretty)
import Test.Hspec

import Cacco.Syntax.AST
import Cacco.Syntax.Index
import Cacco.Syntax.Literal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

spec_as_instance_of_Eq :: Spec
spec_as_instance_of_Eq = do
    it "_ == _" $ Hole ExprProxy == Hole ExprProxy `shouldBe` True

    it "... = ..." $ Dots ExprProxy == Dots ExprProxy `shouldBe` True

spec_as_instance_of_Pretty :: Spec
spec_as_instance_of_Pretty = do
    it "_" $ show (pretty $ Hole ExprProxy) `shouldBe` "_"

    it "..." $ show (pretty $ Dots ExprProxy) `shouldBe` "..."

    it "1" $ show (pretty $ Lit ExprProxy $ Natural 1) `shouldBe` "1"

    it "x" $ show (pretty $ Var ExprProxy "x") `shouldBe` "x"

    it "[1 2 3]" $ do
        let vec =
                Vec
                    [ Lit ExprProxy $ Natural 1
                    , Lit ExprProxy $ Natural 2
                    , Lit ExprProxy $ Natural 3
                    ]
        show (pretty vec) `shouldBe` "[1 2 3]"

    it "{x: 100 y: 200}" $ do
        let ast =
                Map
                    [ (Var ExprProxy "x", Lit ExprProxy $ Natural 100)
                    , (Var ExprProxy "y", Lit ExprProxy $ Natural 200)
                    ]
        show (pretty ast) `shouldBe` "{x: 100 y: 200}"
