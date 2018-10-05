{-# LANGUAGE OverloadedLists #-}

module EvalSpec where

import RIO

import AST
import Eval
import Parse
import Primitives
import Test.Hspec

testEval :: AST -> Prim
testEval = flip runReader primitives . eval

parseEval :: String -> Either String Prim
parseEval = fmap testEval . parse

spec :: Spec
spec = do
  describe "eval" $ do
    it "evals simple arithmetic" $ do
      (parseEval "(+ 1 2)") `shouldBe` Right (PNumber 3)
      (parseEval "(- (+ 1 2) 3)") `shouldBe` Right (PNumber 0)
    it "evals functions" $ do
      (parseEval "({ num (* num 2) } 5)") `shouldBe` Right (PNumber 10)
    -- it "evals literal bindings" $ do
    --   (parseEval "((= :num 5) (+ 1 num))") `shouldBe` Right (Number 6)
    -- it "evals func bindings" $ do
    --   ((parseEval "((= :times_two { num (* num 2) }) (times_two 10))") `shouldBe`
    --    Right (Number 20))
    -- it "evals nested bindings" $ do
    --   ((parseEval
    --       "((= :times_two { num (* num 2) }) ((= :ten (times_two 5)) (+ 5 ten)))") `shouldBe`
    --    Right (Number 15))
    --   (parseEval "((= :times_two { num (* num 2) }) (times_two (times_two 10)))") `shouldBe`
    --     Right (Number 40)
    -- it "builds records" $ do
      -- (parseEval
         -- "({ :times_two : { :num (* num 2) }, inc : { :num (+ 1 num) }, } (times_two (inc 1)))") `shouldBe`
        -- Right (Number 4)
    -- it "concats strings" $ do
    --   (parseEval "(++ \"one\" \"two\")") `shouldBe` Right (Str "onetwo")
    -- it "handles control-flow" $ do
    --   (parseEval "(if T 1 2)") `shouldBe` Right (Number 1)
    --   (parseEval "(if F 1 2)") `shouldBe` Right (Number 2)
    -- it "can test equality" $ do
    --   (parseEval "(== 1 2)") `shouldBe` Right (Boolean False)
    --   (parseEval "(== 1 1)") `shouldBe` Right (Boolean True)
    -- it "can run simple recursive functions" $ do
    --   (parseEval "((= :fact { num (fact 1) }) (fact 0))") `shouldBe`
    --     Right (Number 1)
      -- (parseEval
      --    "((= :fact { num (if (== num 0) 1 (* num (fact (- num 1)))) }) (fact 0))") `shouldBe`
      --   Right (Number 1)
      -- parseEval "((= :func { cond (if cond 5 (func F)) }) (func T))" `shouldBe`
      --   Right (Number 5)
      -- parseEval
      --   "((= :fact { num (if (== num 0) 1 (* num (fact (- num 1)))) }) (fact 1))" `shouldBe`
      --   Right (Number 1)
