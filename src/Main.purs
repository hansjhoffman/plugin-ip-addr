module Main
  ( IPv4
  , parse_
  , toString
  ) where

import Prelude

-- import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either(..))
import Data.Foldable as Data.Foldable
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.String as Data.String
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

newtype IPv4 = IPv4 String

instance showIPv4 :: Show IPv4 where
  show (IPv4 ip) = "(IPv4 " <> show ip <> ")"

instance eqIPv4 :: Eq IPv4 where
  eq (IPv4 ip1) (IPv4 ip2) = ip1 == ip2

toString :: IPv4 -> String
toString (IPv4 ip) = ip

-- | INTERNAL
pOctet :: Parser String String
pOctet = Parsing.String.Basic.takeWhile1 Unicode.isDecDigit
  <?> "a decimal digit"

-- | INTERNAL
isOctetValid :: String -> Boolean
isOctetValid octet =
  fromMaybe false
    $ flip (<=) 255
        <$> Int.fromString octet

-- | INTERNAL
-- |
-- | A parser for any 'reasonable' ip.
parser :: Parser String IPv4
parser = do
  octet1 <- pOctet
  _ <- Parsing.String.char '.'
  octet2 <- pOctet
  _ <- Parsing.String.char '.'
  octet3 <- pOctet
  _ <- Parsing.String.char '.'
  octet4 <- pOctet
  Parsing.String.eof <?> "end of string"

  if (not isOctetValid octet1) then
    Parsing.fail "octet can only be 0-255"
  else if (not isOctetValid octet2) then
    Parsing.fail "octet can only be 0-255"
  else if (not isOctetValid octet3) then
    Parsing.fail "octet can only be 0-255"
  else if (not isOctetValid octet4) then
    Parsing.fail "octet can only be 0-255"
  else
    pure $ mkIPv4 octet1 octet2 octet3 octet4

-- | INTERNAL
mkIPv4 :: String -> String -> String -> String -> IPv4
mkIPv4 octet1 octet2 octet3 octet4 =
  IPv4 $ Data.Foldable.intercalate "." [ octet1, octet2, octet3, octet4 ]

-- | Parse a string as a possible ip.
-- parse :: String -> Either String IP
-- parse = lmap Parsing.parseErrorMessage
--   <<< flip Parsing.runParser parser
--   <<< Str.trim

parse_ :: String -> String
parse_ input = do
  case (flip Parsing.runParser parser $ Data.String.trim input) of
    Left err ->
      Data.String.joinWith "\n" $ Parsing.String.parseErrorHuman input 20 err
    Right ip ->
      toString ip
