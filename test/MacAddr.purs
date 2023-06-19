module Test.MacAddr (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Mac (Eui48(..), MacAddr(..), parse_, print_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- assertRight :: Either String MacAddr -> MacAddr -> Aff Unit
assertRight :: Either String String -> String -> Aff Unit
assertRight eitherActual expected =
  case eitherActual of
    Left _ -> Right expected `shouldEqual` eitherActual
    Right _ -> Right expected `shouldEqual` eitherActual

-- assertLeft :: Either String MacAddr -> String -> Aff Unit
assertLeft :: Either String String -> String -> Aff Unit
assertLeft eitherActual expected =
  case eitherActual of
    Left _ -> Left expected `shouldEqual` eitherActual
    Right _ -> Left expected `shouldEqual` eitherActual

spec :: Spec Unit
spec =
  describe "Mac address parse_" do
    it "should handle valid IPv4 addresses" do
      assertRight (print_ <$> parse_ "ff:ff:ff:ff:ff:ff") ("FF:FF:FF:FF:FF:FF")
      assertRight (print_ <$> parse_ "ff-ff-ff-ff-ff-ff") ("FF-FF-FF-FF-FF-FF")
      assertRight (print_ <$> parse_ "ffff.ffff.ffff") ("FFFF.FFFF.FFFF")
    it "should handle invalid IPv4 addresses" do
      assertLeft (print_ <$> parse_ "f:ff:ff:ff:ff:ff") "Expected hex digit"
      assertLeft (print_ <$> parse_ "ff-ff:ff:ff:ff:ff") "asdf"
    it "should handle valid IPv6 addresses" do
      assertRight (print_ <$> parse_ "ff:ff:ff:ff:ff:ff:ff:ff") ("FF:FF:FF:FF:FF:FF:FF:FF")
      assertRight (print_ <$> parse_ "ff-ff-ff-ff-ff-ff-ff-ff") ("FF-FF-FF-FF-FF-FF-FF-FF")
      assertRight (print_ <$> parse_ "ffff.ffff.ffff.ffff") ("FFFF.FFFF.FFFF.FFFF")
