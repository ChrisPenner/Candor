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
    (Fix $
     Bindings
       [ ( "func"
         , (Fix $
            FuncDef
              ["cond"]
              (Fix $
               Appl
                 (Fix $ Symbol "if")
                 [ Fix $ Symbol "cond"
                 , Fix $ Number 5
                 , (Fix $ Appl (Fix $ Symbol "func") [Fix $ Boolean False])
                 ])))
       ])
    [Fix $ Appl (Fix $ Symbol "func") [Fix $ Boolean True]]

recursiveFactorial :: AST
recursiveFactorial =
  Fix $
  Appl
    (Fix $
     Bindings
       [ ( "fact"
         , (Fix $
            FuncDef
              ["num"]
              (Fix $
               Appl
                 (Fix $ Symbol "if")
                 [ Fix $
                   Appl (Fix $ Symbol "==") [Fix $ Symbol "num", Fix $ Number 0]
                 , Fix $ Number 1
                 , (Fix $
                    Appl
                      (Fix $ Symbol "*")
                      [ Fix $
                        Appl
                          (Fix $ Symbol "fact")
                          [ Fix $
                            Appl
                              (Fix $ Symbol "-")
                              [Fix $ Symbol "num", Fix $ Number 1]
                          ]
                      , Fix $ Symbol "num"
                      ])
                 ])))
       ])
    [Fix $ Appl (Fix $ Symbol "fact") [Fix $ Number 1]]

spec :: Spec
spec = do
  describe "Eval" $ do
    it "evals simple arithmetic" $ do
      (parse "(+ 1 2)" >>= eval) `shouldBe` Right (Fix $ Number 3)
      (parse "(- (+ 1 2) 3)" >>= eval) `shouldBe` Right (Fix $ Number 0)
    it "evals functions" $ do
      (parse "({ [num] (* num 2) } 5)" >>= eval) `shouldBe`
        Right (Fix $ Number 10)
    it "evals literal bindings" $ do
      (parse "((= num 5) (+ 1 num))" >>= eval) `shouldBe` Right (Fix $ Number 6)
    it "evals func bindings" $ do
      ((parse "((= times_two { [num] (* num 2) }) (times_two 10))" >>= eval) `shouldBe`
       Right (Fix $ Number 20))
    it "evals nested bindings" $ do
      ((parse
          "((= times_two { [num] (* num 2) }) ((= ten (times_two 5)) (+ 5 ten)))" >>=
        eval) `shouldBe`
       Right (Fix $ Number 15))
      (parse "((= times_two { [num] (* num 2) }) (times_two (times_two 10)))" >>=
       eval) `shouldBe`
        Right (Fix $ Number 40)
    it "builds records" $ do
      (parse
         "({ times_two : { [num] (* num 2) }, inc : { [num] (+ 1 num) }, } (times_two (inc 1)))" >>=
       eval) `shouldBe`
        Right (Fix $ Number 4)
    it "concats strings" $ do
      (parse "(++ \"one\" \"two\")" >>= eval) `shouldBe`
        Right (Fix $ Str "onetwo")
    it "handles control-flow" $ do
      (parse "(if T 1 2)" >>= eval) `shouldBe` Right (Fix $ Number 1)
      (parse "(if F 1 2)" >>= eval) `shouldBe` Right (Fix $ Number 2)
    it "can test equality" $ do
      (parse "(== 1 2)" >>= eval) `shouldBe` Right (Fix $ Boolean False)
      (parse "(== 1 1)" >>= eval) `shouldBe` Right (Fix $ Boolean True)
    it "can run simple recursive functions" $ do
      (parse
         "((= fact { [num] (if (== num 0) 1 (* num (fact (- num 1)))) }) (fact 0))" >>=
       eval) `shouldBe`
        Right (Fix $ Number 1)
      parse "((= func { [cond] (if cond 5 (func F)) }) (func T))" `shouldBe`
        Right expectedRecursion
      eval expectedRecursion `shouldBe` Right (Fix $ Number 5)
      eval recursiveFactorial `shouldBe` Right (Fix $ Number 1)
