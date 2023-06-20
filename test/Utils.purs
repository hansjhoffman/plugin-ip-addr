module Test.Utils where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)

assertRight :: forall a. Show a => Eq a => Either String a -> a -> Aff Unit
assertRight eitherActual expected =
  case eitherActual of
    Left _ -> Right expected `shouldEqual` eitherActual
    Right _ -> Right expected `shouldEqual` eitherActual

assertLeft :: forall a. Show a => Eq a => Either String a -> String -> Aff Unit
assertLeft eitherActual expected =
  case eitherActual of
    Left _ -> Left expected `shouldEqual` eitherActual
    Right _ -> Left expected `shouldEqual` eitherActual

