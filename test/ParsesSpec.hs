module ParsesSpec where

import qualified Data.Text.IO as TIO
import Test.Hspec
import Parse
import AST
import Data.List.NonEmpty


spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses simple arithmetic" $ do
      parse "(+ 1 2)" `shouldBe` Right (List (Atom (Symbol "+") :| [Atom (Number 1), Atom (Number 2)]))
      parse "(- (+ 1 2) 3)" `shouldBe` Right (List ( Atom (Symbol "-") :| [List (Atom (Symbol "+") :| [Atom (Number 1), Atom (Number 2)]), Atom (Number 3)]))
    it "parses definitions" $ do
      parse "(!times_two {num} (* num 2))" `shouldBe` Right (Decl "times_two" ["num"] (List ((Atom (Symbol "*")) :| [(Atom (Symbol "num")), (Atom (Number 2))])))
    it "parses definitions alongside expressions"
      (parse "((!times_two {num} (* num 2)) (times_two 5))" `shouldBe`
          Right (List ((Decl "times_two" ["num"] (List ((Atom (Symbol "*")) :| [(Atom (Symbol "num")), (Atom (Number 2))]))) :| [List ((Atom (Symbol "times_two")) :| [(Atom (Number 5))])])))


