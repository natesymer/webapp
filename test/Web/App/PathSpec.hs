{-# LANGUAGE OverloadedStrings #-}

module Web.App.PathSpec where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Web.App.Path" $ do
    context "matching" $ do
      it "matches root" $ do
        isRoot "/"              `shouldBe` True
        isRoot "/asdf"          `shouldBe` False
        pathMatches "/"      [] `shouldBe` True
        pathMatches "/asdf"  [] `shouldBe` False
        pathMatches "/:asdf" [] `shouldBe` False
        
      it "matches literal paths" $ do
        pathMatches "/one/two"       ["one", "two"] `shouldBe` True
        pathMatches "/one/two/three" ["one", "two"] `shouldBe` False
        pathMatches "/one"           ["one", "two"] `shouldBe` False
        
      it "matches captured paths" $ do
        pathMatches "/one/:two"       ["one", "two"] `shouldBe` True
        pathMatches "/one/:two/three" ["one", "two"] `shouldBe` False
        pathMatches "/:one"           ["one", "two"] `shouldBe` False
        
      it "matches regex paths" $ do
        pathMatches (regex "\\/\\S+\\/\\S+") ["this", "that"] `shouldBe` True
        pathMatches (regex "\\/\\S+\\/asdf") ["this", "that"] `shouldBe` False

    context "parsing" $ do
      it "parses captures from a captured path" $ do
        pathCaptures (captured "/:model/:attribute") ["foo", "bar"] `shouldBe` [("model", "foo"), ("attribute", "bar")]
        pathCaptures (captured "/:model/")           ["foo"]        `shouldBe` [("model", "foo")]
        pathCaptures (captured "/posts/:id")         ["posts", "1"] `shouldBe` [("id", "1")]
      
      it "parses captures from a regex path" $ do
        pathCaptures (regex "\\/(\\S+)\\/(\\S+)") ["this", "that"] `shouldBe` [("0", joinPathComps ["this", "that"]), ("1", "this"), ("2", "that")]

      it "splits a path into path components" $ do
        splitPathComps "/"                 `shouldBe` []
        splitPathComps "/one"              `shouldBe` ["one"]
        splitPathComps "/one/two"          `shouldBe` ["one", "two"]
        splitPathComps "/one/"             `shouldBe` ["one"]
        splitPathComps "/:group/:resource" `shouldBe` [":group", ":resource"]
        
      it "joins paths components" $ do
        joinPathComps []             `shouldBe` "/"
        joinPathComps ["one"]        `shouldBe` "/one"
        joinPathComps ["one", "two"] `shouldBe` "/one/two"
