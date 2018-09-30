{-# LANGUAGE OverloadedLists #-}

module ParsesSpec where

import RIO

import AST
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses simple arithmetic" $ do
      parse "(+ 1 2)" `shouldBe` Right (apply (sym "+") [num 1, num 2])
      parse "(- (+ 1 2) 3)" `shouldBe`
        Right (apply (sym "-") [apply (sym "+") [num 1, num 2], num 3])
    it "parses bindings" $ do
      parse "(= name 42)" `shouldBe` Right (binding [("name", num 42)])
    it "parses funcs" $ do
      parse "{num (* num 2) }" `shouldBe`
        Right (funcDef "num" (apply (sym "*") [sym "num", num 2]))
    it
      "parses definitions alongside expressions"
      (parse "((= times_two { num (* num 2) }) (times_two 5))" `shouldBe`
       Right
         (apply
            (binding
               [ ( "times_two"
                 , funcDef "num" (apply (sym "*") [sym "num", num 2]))
               ])
            [apply (sym "times_two") [num 5]]))
    it "parses bools" $ do
      parse "F" `shouldBe` Right (boolean False)
      parse "T" `shouldBe` Right (boolean True)
    it "parses strings" $ do parse "\"string\"" `shouldBe` Right (str "string")
