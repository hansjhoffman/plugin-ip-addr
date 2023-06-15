module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse" do
    it "should handle valid IPv4 addresses" do
      (parse_ "127.0.0.1") `shouldEqual` "127.0.0.1"
      (parse_ "0.0.0.0") `shouldEqual` "0.0.0.0"
      (parse_ "255.255.255.255") `shouldEqual` "255.255.255.255"
    it "should handle invalid IPv4 addresses" do
      (parse_ "256.255.255.255") `shouldEqual` "octet can only be 0-255 at position index:15 (line:1, column:16)\n               ▼\n256.255.255.255"
      (parse_ "255.256.255.255") `shouldEqual` "octet can only be 0-255 at position index:15 (line:1, column:16)\n               ▼\n255.256.255.255"
      (parse_ "255.255.256.255") `shouldEqual` "octet can only be 0-255 at position index:15 (line:1, column:16)\n               ▼\n255.255.256.255"
      (parse_ "255.255.255.256") `shouldEqual` "octet can only be 0-255 at position index:15 (line:1, column:16)\n               ▼\n255.255.255.256"
