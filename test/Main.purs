module Test.Main where

import Prelude

import Data.Either (fromLeft, fromRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (parse, toString)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- | https://en.wikipedia.org/wiki/Email_address
main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "parse" do
    let
      dummyMsg = "foobar"
    it "should handle 'simple@example.com'" do
      let
        actual = parse "simple@example.com"
        expected = "simple@example.com"
      (fromRight dummyMsg $ toString <$> actual) `shouldEqual` expected
    it "should handle 'very.common@example.com'" do
      let
        actual = parse "very.common@example.com"
        expected = "very.common@example.com"
      (fromRight dummyMsg $ toString <$> actual) `shouldEqual` expected
    it "should handle 'disposable.style.email.with+symbol@example.com'" do
      let
        actual = parse "disposable.style.email.with+symbol@example.com"
        expected = "disposable.style.email.with+symbol@example.com"
      (fromRight dummyMsg $ toString <$> actual) `shouldEqual` expected

    it "should fail on 'Abc.example.com'" do
      let
        actual = parse "Abc.example.com"
        expected = "Expected '@'"
      (fromLeft dummyMsg $ toString <$> actual) `shouldEqual` expected
    it "should fail on 'A@b@c@example.com'" do
      let
        actual = parse "A@b@c@example.com"
        expected = "Expected '.'"
      (fromLeft dummyMsg $ toString <$> actual) `shouldEqual` expected
    it "should fail on '1234567890123456789012345678901234567890123456789012345678901234+x@example.com'" do
      let
        actual = parse "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
        expected = "Expected '.'" -- | (local-part is longer than 64 characters)
      (fromLeft dummyMsg $ toString <$> actual) `shouldEqual` expected
    it "should fail on 'i_like_underscore@but_its_not_allowed_in_this_part.example.com'" do
      let
        actual = parse "i_like_underscore@but_its_not_allowed_in_this_part.example.com"
        expected = "Expected '.'" -- | (Underscore is not allowed in domain part)
      (fromLeft dummyMsg $ toString <$> actual) `shouldEqual` expected
