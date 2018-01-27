module EvalSpec where

import qualified Data.Text.IO as TIO
import Test.Hspec
import Parse
import Eval
import AST
import Data.List.NonEmpty


spec :: Spec
spec = do
  describe "Eval" $ do
    it "evals simple arithmetic" $ do
      eval <$> (parse "(+ 1 2)") `shouldBe` Right (Number 3)
      eval <$> (parse "(- (+ 1 2) 3)") `shouldBe` Right (Number 0)
