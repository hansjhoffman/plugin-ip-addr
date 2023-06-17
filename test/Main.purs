module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Main (IPv4(..), parse_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

assertRight :: Either String IPv4 -> IPv4 -> Aff Unit
assertRight eitherActual expected =
  case eitherActual of
    Left _ -> Right expected `shouldEqual` eitherActual
    Right _ -> Right expected `shouldEqual` eitherActual

assertLeft :: Either String IPv4 -> String -> Aff Unit
assertLeft eitherActual expected =
  case eitherActual of
    Left _ -> Left expected `shouldEqual` eitherActual
    Right _ -> Left expected `shouldEqual` eitherActual

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "parse_" do
    it "should handle valid IPv4 addresses" do
      assertRight (parse_ "127.0.0.1")
        $ IPv4 "127.0.0.1"
      assertRight (parse_ "0.0.0.0")
        $ IPv4 "0.0.0.0"
      assertRight (parse_ "255.255.255.255")
        $ IPv4 "255.255.255.255"
    it "should handle invalid IPv4 addresses" do
      assertLeft (parse_ "127.0.0.1a")
        $ "Expected end of string starting at position 10"
      assertLeft (parse_ "256.255.255.255")
        $ "Octet can only be 0-255 starting at position 1"
      assertLeft (parse_ "255.256.255.255")
        $ "Octet can only be 0-255 starting at position 5"
      assertLeft (parse_ "255.255.256.255")
        $ "Octet can only be 0-255 starting at position 9"
      assertLeft (parse_ "255.255.255.256")
        $ "Octet can only be 0-255 starting at position 13"
