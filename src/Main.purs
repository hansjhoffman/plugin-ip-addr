module Main
  ( IPv4(..)
  , parse_
  , toString_
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as Data.String
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

newtype IPv4 = IPv4 String

derive instance eqIPv4 :: Eq IPv4

derive instance newtypeIPv4 :: Newtype IPv4 _

derive instance genericIPv4 :: Generic IPv4 _

instance showIPv4 :: Show IPv4 where
  show = genericShow

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
    Parsing.failWithPosition "Octet can only be 0-255" $
      Parsing.Position { column: 1, index: 0, line: 1 }
  else if (not isOctetValid octet2) then
    Parsing.failWithPosition "Octet can only be 0-255" $
      Parsing.Position { column: 5, index: 4, line: 1 }
  else if (not isOctetValid octet3) then
    Parsing.failWithPosition "Octet can only be 0-255" $
      Parsing.Position { column: 9, index: 8, line: 1 }
  else if (not isOctetValid octet4) then
    Parsing.failWithPosition "Octet can only be 0-255" $
      Parsing.Position { column: 13, index: 12, line: 1 }
  else
    pure $ IPv4
      ( intercalate "."
          [ octet1
          , octet2
          , octet3
          , octet4
          ]
      )

-- | INTERNAL
prettyError :: Parsing.ParseError -> String
prettyError err = msg <> " starting at position " <> show col
  where
  msg = Parsing.parseErrorMessage err
  Parsing.Position { column: col, index: _, line: _ } = Parsing.parseErrorPosition err

-- | Parse a string as a possible IPv4 address.
parse_ :: String -> Either String IPv4
parse_ = lmap prettyError
  <<< flip Parsing.runParser parser
  <<< Data.String.trim

-- | Unwraps a IPv4 type
toString_ :: IPv4 -> String
toString_ = unwrap
