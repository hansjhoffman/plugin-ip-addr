module Test.Main where

import Prelude

import Data.Either (fromLeft, fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse_, toString)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  let
    dummyMsg = "foobar"
  describe "parse_" do
    it "should handle valid IPv4 addresses" do
      (fromRight dummyMsg $ toString <$> parse_ "127.0.0.1") `shouldEqual` "127.0.0.1"
      (fromRight dummyMsg $ toString <$> parse_ "0.0.0.0") `shouldEqual` "0.0.0.0"
      (fromRight dummyMsg $ toString <$> parse_ "255.255.255.255") `shouldEqual` "255.255.255.255"
    it "should handle invalid IPv4 addresses" do
      (fromLeft dummyMsg $ parse_ "127.0.0.1a") `shouldEqual` "Expected end of string starting at position 10"
      (fromLeft dummyMsg $ parse_ "256.255.255.255") `shouldEqual` "Octet can only be 0-255 starting at position 1"
      (fromLeft dummyMsg $ parse_ "255.256.255.255") `shouldEqual` "Octet can only be 0-255 starting at position 5"
      (fromLeft dummyMsg $ parse_ "255.255.256.255") `shouldEqual` "Octet can only be 0-255 starting at position 9"
      (fromLeft dummyMsg $ parse_ "255.255.255.256") `shouldEqual` "Octet can only be 0-255 starting at position 13"
