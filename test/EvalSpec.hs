{-# language OverloadedLists #-}
module EvalSpec where

import RIO

import Test.Hspec
import Parse
import Eval
import AST

expectedRecursion :: AST
expectedRecursion = Appl (Appl (Symbol "=") [Binder "func", (Appl (Symbol "def") [List [ Binder "cond" ], (Appl (Symbol "if") [Symbol "cond", Number 5, (Appl (Symbol "func") [Boolean False])])])]) [Appl (Symbol "func") [Boolean True]]

recursiveFactorial :: AST
recursiveFactorial = Appl (Appl (Symbol "=") [Binder "fact", (Appl (Symbol "def") [List [ Binder "num" ], (Appl (Symbol "if") [Appl (Symbol "==") [Symbol "num", Number 0], Number 1, (Appl (Symbol "*") [Appl (Symbol "fact") [Appl (Symbol "-") [Symbol "num", Number 1]], Symbol "num"])])])]) [Appl (Symbol "fact") [Number 1]]

spec :: Spec
spec = do
  describe "Eval" $ do
    it "evals simple arithmetic" $ do
      (parse "(+ 1 2)" >>= eval) `shouldBe` Right (Number 3)
      (parse "(- (+ 1 2) 3)" >>= eval) `shouldBe` Right (Number 0)
    it "evals functions" $ do
      (parse "((def [:num] (* num 2)) 5)" >>= eval) `shouldBe` Right (Number 10)
    it "evals literal bindings" $ do
      (parse "((= :num 5) (+ 1 num))" >>= eval) `shouldBe` Right (Number 6)
    it "evals func bindings" $ do
      ((parse "((= :times_two (def [:num] (* num 2))) (times_two 10))" >>= eval) `shouldBe`
        Right (Number 20))
    it "evals nested bindings" $ do
      ((parse "((= :times_two (def [:num] (* num 2))) ((= :ten (times_two 5)) (+ 5 ten)))" >>= eval) `shouldBe` Right (Number 15))
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
    it "can run simple recursive functions" $ do
      (parse "((= :fact (def [:num] (if (== num 0) 1 (* num (fact (- num 1)))))) (fact 0))" >>= eval) `shouldBe` Right (Number 1)
      parse "((= :func (def [:cond] (if cond 5 (func F)))) (func T))" `shouldBe` Right expectedRecursion
      eval expectedRecursion `shouldBe` Right (Number 5)
      eval recursiveFactorial `shouldBe` Right (Number 1)
