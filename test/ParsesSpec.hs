module ParsesSpec where

import RIO

import Test.Hspec
import Parse
import AST

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses simple arithmetic" $ do
      parse "(+ 1 2)" `shouldBe` Right (Appl (Symbol "+") [Number 1, Number 2])
      parse "(- (+ 1 2) 3)" `shouldBe` Right (Appl (Symbol "-") [Appl (Symbol "+") [Number 1, Number 2], Number 3])
    it "parses funcs" $ do
      parse "(def [:num] (* num 2))" `shouldBe` Right (Appl (Symbol "def") [List [Binder "num"], (Appl (Symbol "*") [Symbol "num", Number 2])])
    it "parses definitions alongside expressions"
      (parse "((= :times_two (def [:num] (* num 2))) (times_two 5))" `shouldBe`
          Right (Appl  (Appl (Symbol "=") [Binder "times_two", Appl (Symbol "def") [List [Binder "num"], (Appl (Symbol "*") [Symbol "num", Number 2])]]) [Appl (Symbol "times_two") [Number 5]]))
    it "parses bools" $ do
      parse "F" `shouldBe` Right (Boolean False)
      parse "T" `shouldBe` Right (Boolean True)
    it "parses strings" $ do
      parse "\"string\"" `shouldBe` Right (Str "string")
