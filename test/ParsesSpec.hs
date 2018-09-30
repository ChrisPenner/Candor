module ParsesSpec where

import RIO

import AST
import qualified Data.Map as M
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses simple arithmetic" $ do
      parse "(+ 1 2)" `shouldBe` Right (curryApply (sym "+") [num 1, num 2])
      parse "(- (+ 1 2) 3)" `shouldBe`
        Right
          (curryApply (sym "-") [curryApply (sym "+") [num 1, num 2], num 3])
    -- it "parses bindings" $ do
    --   parse "(= name 42)" `shouldBe`
    --     Right (binding $ M.fromList [("name", num 42)])
    it "parses bindings" $ do
      parse "(= :name 42)" `shouldBe`
        Right (curryApply (sym "=") [bindSym "name", num 42])
    it "parses funcs" $ do
      parse "{num (* num 2) }" `shouldBe`
        Right (funcDef "num" (curryApply (sym "*") [sym "num", num 2]))
    -- it
    --   "parses definitions alongside expressions"
    --   (parse "((= times_two { num (* num 2) }) (times_two 5))" `shouldBe`
    --    Right
    --      (curryApply
    --         (binding $
    --          M.fromList
    --            [ ( "times_two"
    --              , funcDef "num" (curryApply (sym "*") [sym "num", num 2]))
    --            ])
    --         [curryApply (sym "times_two") [num 5]]))
    it "parses bools" $ do
      parse "F" `shouldBe` Right (boolean False)
      parse "T" `shouldBe` Right (boolean True)
    it "parses strings" $ do parse "\"string\"" `shouldBe` Right (str "string")
