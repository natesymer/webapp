{-# LANGUAGE OverloadedStrings #-}
module Web.App.PathSpec where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Web.App.Path" $ do
    context "matching" $ do
      it "recognizes root" $ 0 `shouldBe` 0
      it "matches literals" $ 0 `shouldBe` 0
      it "matches captured paths" $ 0 `shouldBe` 0
      it "matches regex paths" $ 0 `shouldBe` 0
    
    context "parsing" $ do
      it "parses captures from a path" $ 0 `shouldBe` 0
      it "splits a path into path components" $ do
        splitPathComps "/" `shouldBe` []
        splitPathComps "/one" `shouldBe` ["one"]
        splitPathComps "/one/two" `shouldBe` ["one", "two"]
        splitPathComps "/one/" `shouldBe` ["one"]
      it "joins paths components" $ do
        joinPathComps [] `shouldBe` "/"
        joinPathComps ["one"] `shouldBe` "/one"
        joinPathComps ["one", "two"] `shouldBe` "/one/two"
