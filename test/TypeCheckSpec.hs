module TypeCheckSpec where

import RIO

import Test.Hspec
import Parse
import TypeCheck
import AST

spec :: Spec
spec = do
  describe "typecheck" $ do
    it "checks simple arithmetic" $ do
      runTypecheck <$> parse "(+ 1 2)" `shouldBe` Right TNumber
      runTypecheck <$> parse "(- (+ 1 2) 3)" `shouldBe` Right TNumber
    -- it "parses funcs" $ do
      -- runTypecheck <$> parse "{ [:num] (* num 2) }" `shouldBe` Right (TFunc [TNumber, TNumber])
    -- it "parses definitions alongside expressions"
      -- (parse "((= :times_two {[:num] (* num 2)}) (times_two 5))" `shouldBe`
      --     Right (Appl  (Appl (Symbol "=") [Binder "times_two", FuncDef [Binder "num"] (Appl (Symbol "*") [Symbol "num", Number 2])]) [Appl (Symbol "times_two") [Number 5]]))
    -- it "parses bools" $ do
      -- parse "F" `shouldBe` Right (Boolean False)
      -- parse "T" `shouldBe` Right (Boolean True)
    -- it "parses strings" $ do
      -- parse "\"string\"" `shouldBe` Right (Str "string")
