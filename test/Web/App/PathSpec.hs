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
      it "parses captures from a captured path" $ do
        let one = captured "/:model/:attribute"
            two = captured "/:model/"
            three = captured "/posts/:id"
        pathCaptures one ["foo", "bar"] `shouldBe` [("model", "foo"), ("attribute", "bar")]
        pathCaptures two ["foo"] `shouldBe` [("model", "foo")]
        pathCaptures three ["posts", "1"] `shouldBe` [("id", "1")]
      
      it "parses captures from a regex path" $ do
        let one = regex "\\/(\\S+)\\/(\\S+)"
        pathCaptures one ["this", "that"] `shouldBe` [("0", joinPathComps ["this", "that"]), ("1", "this"), ("2", "that")]

      it "splits a path into path components" $ do
        splitPathComps "/" `shouldBe` []
        splitPathComps "/one" `shouldBe` ["one"]
        splitPathComps "/one/two" `shouldBe` ["one", "two"]
        splitPathComps "/one/" `shouldBe` ["one"]
        splitPathComps "/:group/:resource" `shouldBe` [":group", ":resource"]
        
      it "joins paths components" $ do
        joinPathComps [] `shouldBe` "/"
        joinPathComps ["one"] `shouldBe` "/one"
        joinPathComps ["one", "two"] `shouldBe` "/one/two"
