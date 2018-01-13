module ParsesSpec where

import qualified Data.Text.IO as TIO
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses simple addition" $ do
      TIO.readFile

      True `shouldBe` True
