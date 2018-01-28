module ParsesSpec where

import RIO

import Test.Hspec
import Parse
import AST
import Data.List.NonEmpty


spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses simple arithmetic" $ do
      parse "(+ 1 2)" `shouldBe` Right (Appl (Symbol "+" :| [Number 1, Number 2]))
      parse "(- (+ 1 2) 3)" `shouldBe` Right (Appl (Symbol "-" :| [Appl (Symbol "+" :| [Number 1, Number 2]), Number 3]))
    it "parses funcs" $ do
      parse "{ [num] (* num 2) }" `shouldBe` Right (Func [Symbol "num"] (Appl (Symbol "*" :| [Symbol "num", Number 2])))
    -- it "parses definitions alongside expressions"
      -- (parse "((!{num} (* num 2)) (times_two 5))" `shouldBe`
          -- Right (Appl ((Decl "times_two" ["num"] (Appl (Symbol "*" :| [Symbol "num", Number 2]))) :| [Appl (Symbol "times_two" :| [Number 5])])))
