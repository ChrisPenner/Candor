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
      (parse "((def [:num] (* num 2)) 5)" >>= eval) `shouldBe` Right (Number 10)
    it "evals bindings" $ do
      ((parse "((= :times_two (def [:num] (* num 2))) (times_two 10))" >>= eval) `shouldBe`
        Right (Number 20))
    it "evals recursive functions" $ do
      (parse "((= :times_two (def [:num] (* num 2))) (times_two (times_two 10)))" >>= eval) `shouldBe` Right (Number 40)
    it "merges bindings" $ do
      (parse "((merge [(= :times_two (def [:num] (* num 2))) (= :inc (def [:num] (+ 1 num)))]) (times_two (inc 1)))" >>= eval) `shouldBe` Right (Number 4)
    it "concats strings" $ do
      (parse "(++ \"one\" \"two\")" >>= eval) `shouldBe` Right (Str "onetwo")
    it "handles control-flow" $ do
      (parse "(if T 1 2)" >>= eval) `shouldBe` Right (Number 1)
      (parse "(if F 1 2)" >>= eval) `shouldBe` Right (Number 2)
    it "can test equality" $ do
      (parse "(== 1 2)" >>= eval) `shouldBe` Right (Boolean False)
      (parse "(== 1 1)" >>= eval) `shouldBe` Right (Boolean True)
    -- it "can run simple recursive functions" $ do
      -- (parse "((= :fact (def [:num] (if (== num 0) 1 (* num (fact (- num 1)))))) (fact 5))" >>= eval) `shouldBe` Right (Number 1)


