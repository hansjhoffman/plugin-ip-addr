module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse" do
    it "should handle '127.0.0.1'" do
      let
        actual = parse "127.0.0.1"
        expected = "127.0.0.1"
      actual `shouldEqual` expected
    it "should handle '0.0.0.0'" do
      let
        actual = parse "0.0.0.0"
        expected = "0.0.0.0"
      actual `shouldEqual` expected
    it "should handle '255.255.255.255'" do
      let
        actual = parse "255.255.255.255"
        expected = "255.255.255.255"
      actual `shouldEqual` expected
