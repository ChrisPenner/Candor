{-# LANGUAGE OverloadedLists #-}

module EvalSpec where

import RIO

import AST
import Data.Functor.Foldable
import Eval
import Parse
import Test.Hspec

expectedRecursion :: AST
expectedRecursion =
  Fix $
  Appl
    (binding
       [ ( "func"
         , (Fix $
            FuncDef
              ["cond"]
              (Fix $
               Appl
                 (sym "if")
                 [sym "cond", num 5, (Fix $ Appl (sym "func") [boolean False])])))
       ])
    [Fix $ Appl (sym "func") [boolean True]]

recursiveFactorial :: AST
recursiveFactorial =
  Fix $
  Appl
    (binding
       [ ( "fact"
         , (Fix $
            FuncDef
              ["num"]
              (Fix $
               Appl
                 (sym "if")
                 [ Fix $ Appl (sym "==") [sym "num", num 0]
                 , num 1
                 , (Fix $
                    Appl
                      (sym "*")
                      [ Fix $
                        Appl
                          (sym "fact")
                          [Fix $ Appl (sym "-") [sym "num", num 1]]
                      , sym "num"
                      ])
                 ])))
       ])
    [Fix $ Appl (sym "fact") [num 1]]

spec :: Spec
spec = do
  describe "Eval" $ do
    it "evals simple arithmetic" $ do
      (eval <$> parse "(+ 1 2)") `shouldBe` Right (num 3)
      (eval <$> parse "(- (+ 1 2) 3)") `shouldBe` Right (num 0)
    -- it "evals functions" $ do
    --   (parse "({ [num] (* num 2) } 5)" >>= eval) `shouldBe` Right (num 10)
    -- it "evals literal bindings" $ do
    --   (parse "((= num 5) (+ 1 num))" >>= eval) `shouldBe` Right (num 6)
    -- it "evals func bindings" $ do
    --   ((parse "((= times_two { [num] (* num 2) }) (times_two 10))" >>= eval) `shouldBe`
    --    Right (num 20))
    -- it "evals nested bindings" $ do
    --   ((parse
    --       "((= times_two { [num] (* num 2) }) ((= ten (times_two 5)) (+ 5 ten)))" >>=
    --     eval) `shouldBe`
    --    Right (num 15))
    --   (parse "((= times_two { [num] (* num 2) }) (times_two (times_two 10)))" >>=
    --    eval) `shouldBe`
    --     Right (num 40)
    -- it "builds records" $ do
    --   (parse
    --      "({ times_two : { [num] (* num 2) }, inc : { [num] (+ 1 num) }, } (times_two (inc 1)))" >>=
    --    eval) `shouldBe`
    --     Right (num 4)
    -- it "concats strings" $ do
    --   (parse "(++ \"one\" \"two\")" >>= eval) `shouldBe`
    --     Right (Fix $ Str "onetwo")
    -- it "handles control-flow" $ do
    --   (parse "(if T 1 2)" >>= eval) `shouldBe` Right (num 1)
    --   (parse "(if F 1 2)" >>= eval) `shouldBe` Right (num 2)
    -- it "can test equality" $ do
    --   (parse "(== 1 2)" >>= eval) `shouldBe` Right (boolean False)
    --   (parse "(== 1 1)" >>= eval) `shouldBe` Right (boolean True)
    -- it "can run simple recursive functions" $ do
    --   (parse
    --      "((= fact { [num] (if (== num 0) 1 (* num (fact (- num 1)))) }) (fact 0))" >>=
    --    eval) `shouldBe`
    --     Right (num 1)
    --   parse "((= func { [cond] (if cond 5 (func F)) }) (func T))" `shouldBe`
    --     Right expectedRecursion
    --   eval expectedRecursion `shouldBe` Right (num 5)
    --   eval recursiveFactorial `shouldBe` Right (num 1)
