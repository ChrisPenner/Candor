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
      parse "(+ 1 2)" `shouldBe` Right (Appl (Atom (Symbol "+") :| [Atom (Number 1), Atom (Number 2)]))
      parse "(- (+ 1 2) 3)" `shouldBe` Right (Appl ( Atom (Symbol "-") :| [Appl (Atom (Symbol "+") :| [Atom (Number 1), Atom (Number 2)]), Atom (Number 3)]))
    it "parses funcs" $ do
      parse "{ [num] (* num 2) }" `shouldBe` Right (Atom (Func [Atom (Symbol "num")] (Appl ((Atom (Symbol "*")) :| [(Atom (Symbol "num")), (Atom (Number 2))]))))
    -- it "parses definitions alongside expressions"
      -- (parse "((!{num} (* num 2)) (times_two 5))" `shouldBe`
          -- Right (Appl ((Decl "times_two" ["num"] (Appl ((Atom (Symbol "*")) :| [(Atom (Symbol "num")), (Atom (Number 2))]))) :| [Appl ((Atom (Symbol "times_two")) :| [(Atom (Number 5))])])))


