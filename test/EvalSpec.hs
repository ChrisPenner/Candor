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
      eval <$> (parse "(+ 1 2)") `shouldBe` Right (Number 3)
      eval <$> (parse "(- (+ 1 2) 3)") `shouldBe` Right (Number 0)
    it "evals functions" $ do
      eval <$> parse "({[:num] (* num 2)} 5)" `shouldBe` Right (Number 10)
    it "evals bindings" $ do
      ((eval <$> parse "((= :times_two {[:num] (* num 2)}) (times_two 10))") `shouldBe`
        Right (Number 20))
    it "evals recursive functions" $ do
      eval <$> parse "((= :times_two {[:num] (* num 2)}) (times_two (times_two 10)))" `shouldBe` Right (Number 40)
    it "merges bindings" $ do
      eval <$> parse "((merge [(= :times_two {[:num] (* num 2)}) (= :inc {[:num] (+ 1 num)})]) (times_two (inc 1)))" `shouldBe` Right (Number 4)
