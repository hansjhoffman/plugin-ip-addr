module Main
  ( IPv4(..)
  , parse_
  , toString_
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Parsing (Parser, ParseError, Position(..))
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String (char, eof)
import Parsing.String.Basic (takeWhile1)

newtype IPv4 = IPv4 String

derive instance eqIPv4 :: Eq IPv4

derive instance newtypeIPv4 :: Newtype IPv4 _

derive instance genericIPv4 :: Generic IPv4 _

instance showIPv4 :: Show IPv4 where
  show = genericShow

-- | INTERNAL
-- |
-- | A parser for an IP address according to the spec:
-- | https://...
parser :: Parser String IPv4
parser = do
  let
    isOctetValid :: String -> Boolean
    isOctetValid octet =
      fromMaybe false
        $ flip (<=) 255
            <$> Int.fromString octet
  octet1 <- takeWhile1 isDecDigit <?> "a decimal digit"
  _ <- char '.'
  octet2 <- takeWhile1 isDecDigit <?> "a decimal digit"
  _ <- char '.'
  octet3 <- takeWhile1 isDecDigit <?> "a decimal digit"
  _ <- char '.'
  octet4 <- takeWhile1 isDecDigit <?> "a decimal digit"
  eof <?> "end of string"

  if (not isOctetValid octet1) then
    Parsing.failWithPosition "Octet can only be 0-255" $
      Position { column: 1, index: 0, line: 1 }
  else if (not isOctetValid octet2) then
    Parsing.failWithPosition "Octet can only be 0-255" $
      Position { column: 5, index: 4, line: 1 }
  else if (not isOctetValid octet3) then
    Parsing.failWithPosition "Octet can only be 0-255" $
      Position { column: 9, index: 8, line: 1 }
  else if (not isOctetValid octet4) then
    Parsing.failWithPosition "Octet can only be 0-255" $
      Position { column: 13, index: 12, line: 1 }
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
-- |
-- | Give the end user enough information about not only
-- | WHAT went wrong, but also WHERE it wrong.
prettyError :: ParseError -> String
prettyError err = msg <> " starting at position " <> show col
  where
  msg = Parsing.parseErrorMessage err
  Position { column: col, index: _, line: _ } = Parsing.parseErrorPosition err

-- | Parse a string as a possible IPv4 address.
parse_ :: String -> Either String IPv4
parse_ = lmap prettyError
  <<< flip Parsing.runParser parser
  <<< String.trim

-- | Unwraps an IPv4 type
toString_ :: IPv4 -> String
toString_ = unwrap
