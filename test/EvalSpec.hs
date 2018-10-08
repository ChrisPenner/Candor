{-# LANGUAGE OverloadedLists #-}

module EvalSpec where

import RIO

import AST
import Data.Functor.Foldable
import Eval
import Parse
import Test.Hspec

testEval :: AST -> NoBindingsAST
testEval = flip runReader mempty . eval

parseEval :: String -> Either String NoBindingsAST
parseEval = fmap testEval . parse

spec :: Spec
spec = do
  describe "eval" $ do
    it "evals simple arithmetic" $ do
      (parseEval "(+ 20 10)") `shouldBe` Right (FNNumber 30)
      (parseEval "(- (+ 5 5) 3)") `shouldBe` Right (FNNumber 7)
    it "evals functions" $ do
      (parseEval "({ num (+ num 2) } 5)") `shouldBe` Right (FNNumber 7)
    it "evals literal bindings" $ do
      (parseEval "((= :num 5) (+ 1 num))") `shouldBe` Right (FNNumber 6)
    it "evals func bindings" $ do
      ((parseEval "((= :times_two { num (* num 2) }) (times_two 10))") `shouldBe`
       Right (FNNumber 20))
    it "evals nested bindings" $ do
      ((parseEval
          "((= :times_two { num (* num 2) }) ((= :ten (times_two 5)) (+ 5 ten)))") `shouldBe`
       Right (FNNumber 15))
      (parseEval "((= :times_two { num (* num 2) }) (times_two (times_two 10)))") `shouldBe`
        Right (FNNumber 40)
    it "handles control-flow" $ do
      (parseEval "(if T 1 2)") `shouldBe` Right (FNNumber 1)
      (parseEval "(if F 1 2)") `shouldBe` Right (FNNumber 2)
    it "can test equality" $ do
      (parseEval "(== 1 2)") `shouldBe` Right (Fix $ NBoolean False)
      (parseEval "(== 1 1)") `shouldBe` Right (Fix $ NBoolean True)
    it "can handle ignored recursion" $ do
      (parseEval "((= :func { n (if T 42 (func 0))}) (func 0))") `shouldBe`
        Right (Fix $ NNumber 42)
    it "can run recurse once" $ do
      (parseEval "((= :fact { num (if (== num 0) 1 ($ (- num 1))) }) (fact 1))") `shouldBe`
        Right (Fix $ NNumber 1)
    it "can run recurse many times" $ do
      (parseEval
         "((= :fact { num (if (== num 0) 1 (* num ($ (- num 1)))) }) (fact 3))") `shouldBe`
        Right (Fix $ NNumber 6)
      -- (parseEval
      --    "((= :fact { num (if (== num 0) 1 (* num (fact (- num 1)))) }) (fact 0))") `shouldBe`
      --   Right (Number 1)
      -- parseEval "((= :func { cond (if cond 5 (func F)) }) (func T))" `shouldBe`
      --   Right (Number 5)
      -- parseEval
      --   "((= :fact { num (if (== num 0) 1 (* num (fact (- num 1)))) }) (fact 1))" `shouldBe`
      --   Right (Number 1)
    -- it "builds records" $ do
    --   (parseEval
    --      "({ :times_two : { :num (* num 2) }, inc : { :num (+ 1 num) }, } (times_two (inc 1)))") `shouldBe`
    --     Right (FNNumber 4)
    -- it "concats strings" $ do
    --   (parseEval "(++ \"one\" \"two\")") `shouldBe` Right (Str "onetwo")
