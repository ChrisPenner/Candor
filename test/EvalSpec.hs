module EvalSpec where

import RIO

import Test.Hspec
import Parse
import Eval
import AST


spec :: Spec
spec = do
  describe "Eval" $ do
    it "evals simple arithmetic" $ do
      (parse "(+ 1 2)" >>= eval) `shouldBe` Right (Number 3)
      (parse "(- (+ 1 2) 3)" >>= eval) `shouldBe` Right (Number 0)
    it "evals functions" $ do
      (parse "({[:num] (* num 2)} 5)" >>= eval) `shouldBe` Right (Number 10)
    it "evals bindings" $ do
      ((parse "((= :times_two {[:num] (* num 2)}) (times_two 10))" >>= eval) `shouldBe`
        Right (Number 20))
    it "evals recursive functions" $ do
      (parse "((= :times_two {[:num] (* num 2)}) (times_two (times_two 10)))" >>= eval) `shouldBe` Right (Number 40)
    it "merges bindings" $ do
      (parse "((merge [(= :times_two {[:num] (* num 2)}) (= :inc {[:num] (+ 1 num)})]) (times_two (inc 1)))" >>= eval) `shouldBe` Right (Number 4)
