module TypeInferenceSpec where

import RIO

import Test.Hspec
import AST
import TypeInference

spec :: Spec
spec = do
  describe "TypeInference" $ do
    describe "unify" $ do
      describe "unifies monotypes" $ do
        it "unifies TConsts" $ do
          runInference (unify intT intT) `shouldBe` Right mempty
