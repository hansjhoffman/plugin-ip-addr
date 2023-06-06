module Main
  ( Email
  , parse
  , toString
  ) where

import Prelude

-- import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either)
import Data.String as Str
import Parsing (Parser)
import Parsing as Parsing
-- import Parsing.Combinators ((<?>))
-- import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

newtype Email = Email String

instance showEmail :: Show Email where
  show (Email email) = "(Email " <> show email <> ")"

instance eqEmail :: Eq Email where
  eq (Email email1) (Email email2) = email1 == email2

toString :: Email -> String
toString (Email email) = email

-- | INTERNAL
pLocal :: Parser String String
pLocal = Parsing.String.Basic.takeWhile1 Unicode.isAsciiLower

-- | INTERNAL
pDomain :: Parser String String
pDomain = do
  a <- Parsing.String.Basic.takeWhile1 Unicode.isAsciiLower
  _ <- Parsing.String.char '.'
  b <- Parsing.String.Basic.takeWhile1 Unicode.isAsciiLower
  pure $ a <> "." <> b

-- | INTERNAL
-- |
-- | A parser for any 'reasonable' email.
parser :: Parser String Email
parser = do
  localPart <- pLocal
  _ <- Parsing.String.char '@'
  domain <- pDomain
  Parsing.String.eof
  pure $ Email (localPart <> "@" <> domain)

-- | Parse a string as a possible email.
parse :: String -> Either String Email
parse = lmap Parsing.parseErrorMessage
  <<< flip Parsing.runParser parser
  <<< (Str.toLower <<< Str.trim)
