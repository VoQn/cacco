
module Cacco.EvalSpec where

import qualified Data.Text             as Text
import           Test.Tasty.Hspec

import           Cacco.Core            (builtin)
import           Cacco.Env
import           Cacco.Error
import           Cacco.Eval            (eval)
import           Cacco.Syntax.Location (Location)
import           Cacco.Syntax.Parser   (parseExpr)
import           Cacco.Val             (Val (..))
import qualified Cacco.Val             as Val

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

prelude :: Env (Val Location)
prelude = initEnv { symbols = builtin }

evalTest :: String -> Either (Error Location) (Val Location)
evalTest str = case parseExpr "EvalTest" (Text.pack str) of
  Left e     -> Left $ Message ("parse error" ++ show e) Nothing
  Right expr -> case eval prelude expr of
    (Left err, _)     -> Left err
    (Right result, _) -> Right result

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

spec_gt_operator :: Spec
spec_gt_operator = describe "operator (>)" $ do
  it "eval (> 1 2) => false" $
    evalTest "(> 1 2)" `shouldBe` Right (Val.bool False)

  it "eval (> 2_u8 1_u8) => true" $
    evalTest "(> 2_u8 1_u8)" `shouldBe` Right (Val.bool True)

  it "eval (> 2_u8 1_u8 2_u8) => false" $
    evalTest "(> 2_u8 1_u8 2_u8)" `shouldBe` Right (Val.bool False)

  it "eval (> 4 3 2 1) => true" $
    evalTest "(> 4 3 2 1)" `shouldBe` Right (Val.bool True)

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

  it "eval (+ 0.1 0.2 0.3 0.4) => 1.0" $
    evalTest "(+ 0.1 0.2 0.3 0.4)" `shouldBe` Right (Val.flonum 1)

  it "eval (+ 1_i8 2_i8) => 3_i8" $
    evalTest "(+ 1_i8 2_i8)" `shouldBe` Right (Val.int8 3)

  it "eval (+ 1_i16 2_i16) => 3_i16" $
    evalTest "(+ 1_i16 2_i16)" `shouldBe` Right (Val.int16 3)

  it "eval (+ 1_i32 2_i32) => 3_i32" $
    evalTest "(+ 1_i32 2_i32)" `shouldBe` Right (Val.int32 3)

  it "eval (+ 1_i64 2_i64) => 3_i64" $
    evalTest "(+ 1_i64 2_i64)" `shouldBe` Right (Val.int64 3)

  it "eval (+ 1_u8 2_u8) => 3_u8" $
    evalTest "(+ 1_u8 2_u8)" `shouldBe` Right (Val.uint8 3)

  it "eval (+ 1_u16 2_u16) => 3_u16" $
    evalTest "(+ 1_u16 2_u16)" `shouldBe` Right (Val.uint16 3)

spec_multiply_operator :: Spec
spec_multiply_operator = describe "operator (*)" $ do
  it "eval (* 1 2 3 4 5) => 120" $
    evalTest "(* 1 2 3 4 5)" `shouldBe` Right (Val.integer 120)

  it "eval (* 2.0_f16 0.5_f16) => 1.0_f16" $
    evalTest "(* 2.0_f16 0.5_f16)" `shouldBe` Right (Val.float16 1)

  it "eval (* 2.0_f32 0.5_f32) => 1.0_f32" $
    evalTest "(* 2.0_f32 0.5_f32)" `shouldBe` Right (Val.float32 1)

  it "eval (* 2.0_f64 0.5_f64) => 1.0_f64" $
    evalTest "(* 2.0_f64 0.5_f64)" `shouldBe` Right (Val.float64 1)
