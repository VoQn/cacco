
module Cacco.EvalSpec where

import qualified Data.Text        as Text
import           Test.Tasty.Hspec

import           Cacco.Eval       (eval)
import           Cacco.Location   (Location)
import           Cacco.Parser     (parseExpr)
import           Cacco.Val        (Val (..))
import qualified Cacco.Val        as Val

evalTest :: String -> Either String (Val Location)
evalTest str = case parseExpr "EvalTest" (Text.pack str) of
  Left _ -> Left "parse error"
  Right expr -> case eval expr [] of
    Left err     -> Left err
    Right result -> Right result

spec_boolean :: Spec
spec_boolean = do
  it "eval true => true" $
    evalTest "true" `shouldBe` Right (Val.bool True)
  it "eval false => false" $
    evalTest "false" `shouldBe` Right (Val.bool False)

spec_equal_operator :: Spec
spec_equal_operator = describe "operator (==)" $ do
  it "eval (== true true) => true" $
    evalTest "(== true true)" `shouldBe` Right (Val.bool True)
  it "eval (== true false) => false" $
    evalTest "(== true false)" `shouldBe` Right (Val.bool False)
  it "eval (== false false) => true" $
    evalTest "(== true true)" `shouldBe` Right (Val.bool True)
  it "eval (== 1 1) => true" $
    evalTest "(== 1 1)" `shouldBe` Right (Val.bool True)
  it "eval (== 0 1) => false" $
    evalTest "(== 0 1)" `shouldBe` Right (Val.bool False)

spec_minus_operator :: Spec
spec_minus_operator = describe "operator (-)" $ do
  it "eval (- 1) => -1" $
    evalTest "(- 1)" `shouldBe` Right (Val.integer (-1))
  it "eval (- 1 1) => 0" $
    evalTest "(- 1 1)" `shouldBe` Right (Val.integer 0)

spec_plus_operator :: Spec
spec_plus_operator = describe "operator (+)" $ do
  it "eval (+ 1) => 1" $
    evalTest "(+ 1)" `shouldBe` Right (Val.integer 1)
  it "eval (+ 1 2) => 3" $
    evalTest "(+ 1 2)" `shouldBe` Right (Val.integer 3)
  it "eval (+ 1 2 3 4 5) => 15" $
    evalTest "(+ 1 2 3)" `shouldBe` Right (Val.integer 6)

spec_multiply_operator :: Spec
spec_multiply_operator = describe "operator (*)" $ do
  it "eval (* 1 2 3 4 5) => 120" $
    evalTest "(* 1 2 3 4 5)" `shouldBe` Right (Val.integer 120)
