module Web.App.StreamSpec where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Web.App.Stream" $ do
    context "general" $ do
      it "tests" $ do
        0 `shouldBe` 0