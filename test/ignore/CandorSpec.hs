module CandorSpec where

import RIO

import Test.Hspec

spec :: Spec
spec = do
  describe "Candor" $ do
    it "runs tests" $
      True `shouldBe` True
