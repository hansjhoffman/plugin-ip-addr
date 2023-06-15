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
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse_" do
    it "should handle valid IPv4 addresses" do
      (fromRight "foobar" $ toString <$> parse_ "127.0.0.1") `shouldEqual` "127.0.0.1"
      (fromRight "foobar" $ toString <$> parse_ "0.0.0.0") `shouldEqual` "0.0.0.0"
      (fromRight "foobar" $ toString <$> parse_ "255.255.255.255") `shouldEqual` "255.255.255.255"
    -- all positions are going to be the same b/c the parser technically finished parsing the string
    -- correctly and the error message comes from 'post logic' e.g. octet <= 255
    it "should handle invalid IPv4 addresses" do
      (fromLeft "foobar" $ parse_ "256.255.255.255") `shouldEqual` "octet can only be 0-255 at position 16"
      (fromLeft "foobar" $ parse_ "255.256.255.255") `shouldEqual` "octet can only be 0-255 at position 16"
      (fromLeft "foobar" $ parse_ "255.255.256.255") `shouldEqual` "octet can only be 0-255 at position 16"
      (fromLeft "foobar" $ parse_ "255.255.255.256") `shouldEqual` "octet can only be 0-255 at position 16"
