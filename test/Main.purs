module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Main (IPv4, parse_, unsafeFromInts)
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
        $ unsafeFromInts 127 0 0 1
      assertRight (parse_ "0.0.0.0")
        $ unsafeFromInts 0 0 0 0
      assertRight (parse_ "255.255.255.255")
        $ unsafeFromInts 255 255 255 255
    it "should handle invalid IPv4 addresses" do
      assertLeft (parse_ "127.0.0.1a")
        $ "Expected end of string"
      assertLeft (parse_ "256.255.255.255")
        $ "IPv4 octet out of range: 0 >= n <= 255"
      assertLeft (parse_ "255.256.255.255")
        $ "IPv4 octet out of range: 0 >= n <= 255"
      assertLeft (parse_ "255.255.256.255")
        $ "IPv4 octet out of range: 0 >= n <= 255"
      assertLeft (parse_ "255.255.255.256")
        $ "IPv4 octet out of range: 0 >= n <= 255"
      assertLeft (parse_ "255.255.255.")
        $ "Expected octet to be 1-3 digit(s)"
