module Test.IpAddr (spec) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Main (IPv4, parse_, print_, unsafeFromInts)
import Test.QuickCheck ((===))
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QCG
import Test.Spec (Spec, describe, it)
import Test.Utils (assertLeft)

-- | Generates a random `IPv4` address for testing purposes.
genIPv4 ∷ ∀ m. Gen.MonadGen m ⇒ m IPv4
genIPv4 = do
  o1 <- Gen.chooseInt 0 255
  o2 <- Gen.chooseInt 0 255
  o3 <- Gen.chooseInt 0 255
  o4 <- Gen.chooseInt 0 255
  pure $ unsafeFromInts o1 o2 o3 o4

forAll ∷ ∀ prop. QC.Testable prop ⇒ QCG.Gen prop → Aff Unit
forAll = quickCheck

quickCheck ∷ ∀ prop. QC.Testable prop ⇒ prop → Aff Unit
quickCheck = liftEffect <<< QC.quickCheck' 100

spec :: Spec Unit
spec =
  describe "IPv4 address parse_" do
    it "should successfully roundtrip values sent through IPv4 parse/print" do
      forAll do
        ipv4 <- genIPv4
        let
          printed = print_ ipv4
          parsed = parse_ printed
        pure $ Right ipv4 === parsed
    it "should handle invalid IPv4 addresses" do
      assertLeft (parse_ "127.0.0.1a")
        $ "Expected end of string"
      assertLeft (parse_ "192.168.001.1")
        $ "Expected '.'"
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
