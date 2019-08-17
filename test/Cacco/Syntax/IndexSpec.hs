{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Cacco.Syntax.IndexSpec where

import           Test.Tasty.Hspec

import           Test.SmallCheck                ( Property
                                                , (==>)
                                                )

import           Cacco.Syntax.Index
import           Cacco.Syntax.Index.Series      ( )

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

scprop_as_instance_of_Show :: Monad m => Index -> Index -> Property m
scprop_as_instance_of_Show a b = a /= b ==> show [a] /= show [b]

spec_IndexProxy_instance_of_Eq :: Spec
spec_IndexProxy_instance_of_Eq = do
  it "ExprProxy" $ ExprProxy /= ExprProxy `shouldBe` False
  it "DeclProxy" $ DeclProxy /= DeclProxy `shouldBe` False
  it "PattProxy" $ PattProxy /= PattProxy `shouldBe` False
  it "TypeProxy" $ TypeProxy /= TypeProxy `shouldBe` False

spec_IndexProxy_instance_of_Show :: Spec
spec_IndexProxy_instance_of_Show = do
  it "ExprProxy" $ show ExprProxy `shouldBe` "ExprProxy"
  it "DeclProxy" $ show DeclProxy `shouldBe` "DeclProxy"
  it "PattProxy" $ show PattProxy `shouldBe` "PattProxy"
  it "TypeProxy" $ show TypeProxy `shouldBe` "TypeProxy"

scprop_ExprProxy_instance_of_Show
  :: Monad m => IndexProxy 'Expr -> IndexProxy 'Expr -> Property m
scprop_ExprProxy_instance_of_Show a b = a == b ==> show [a] == show [b]

scprop_DeclProxy_instance_of_Show
  :: Monad m => IndexProxy 'Decl -> IndexProxy 'Decl -> Property m
scprop_DeclProxy_instance_of_Show a b = a == b ==> show [a] == show [b]

scprop_PattProxy_instance_of_Show
  :: Monad m => IndexProxy 'Patt -> IndexProxy 'Patt -> Property m
scprop_PattProxy_instance_of_Show a b = a == b ==> show [a] == show [b]

scprop_TypeProxy_instance_of_Show
  :: Monad m => IndexProxy 'Type -> IndexProxy 'Type -> Property m
scprop_TypeProxy_instance_of_Show a b = a == b ==> show [a] == show [b]
