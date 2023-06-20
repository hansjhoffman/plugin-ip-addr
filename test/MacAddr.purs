module Test.MacAddr (spec) where

import Prelude

import Mac (parse_, print_)
import Test.Spec (Spec, describe, it)
import Test.Utils (assertLeft, assertRight)

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
