module EvalSpec where

import RIO

import Test.Hspec
import Parse
import Eval
import AST


spec :: Spec
spec = do
  describe "Eval" $ do
    it "evals simple arithmetic" $ do
      eval <$> (parse "(+ 1 2)") `shouldBe` Right (Number 3)
      eval <$> (parse "(- (+ 1 2) 3)") `shouldBe` Right (Number 0)
