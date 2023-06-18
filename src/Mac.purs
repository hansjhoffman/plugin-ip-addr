module Mac
  ( Eui48(..)
  , Eui64(..)
  , MacAddr(..)
  , upConvert_
  , parse_
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.CodePoint.Unicode (isHexDigit)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String
import Parsing (Parser, ParseError, Position(..))
import Parsing as Parsing
import Parsing.Combinators (choice, try, (<?>))
import Parsing.String (char, eof)
import Parsing.String.Basic (takeWhile1)

data MacAddr
  = IPv4 Eui48
  | IPv6 Eui64

derive instance eqMacAddr :: Eq MacAddr
derive instance genericMacAddr :: Generic MacAddr _

instance showMacAddr :: Show MacAddr where
  show = genericShow

data Eui48
  = SixGroupsByColon String
  | SixGroupsByHyphen String
  | ThreeGroupsByDot String

derive instance eqEui48 :: Eq Eui48
derive instance genericEui48 :: Generic Eui48 _

instance showEui48 :: Show Eui48 where
  show = genericShow

data Eui64
  = EightGroupsByColon String
  | EightGroupsByHyphen String
  | FourGroupsByDot String

derive instance eqEui64 :: Eq Eui64
derive instance genericEui64 :: Generic Eui64 _

instance showEui64 :: Show Eui64 where
  show = genericShow

octet :: Parser String String
octet = takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"

-- | INTERNAL
pEui48 :: Parser String Eui48
pEui48 = try $ choice
  [ pSixGroupsByColon
  , pSixGroupsByHyphen
  , pThreeGroupsByDot
  ]

-- | INTERNAL
-- |
-- | Format: FF:FF:FF:FF:FF:FF
pSixGroupsByColon :: Parser String Eui48
pSixGroupsByColon = do
  o1 <- octet <* char ':'
  o2 <- octet <* char ':'
  o3 <- octet <* char ':'
  o4 <- octet <* char ':'
  o5 <- octet <* char ':'
  o6 <- octet
  eof <?> "end of string"

  if (String.length o1 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 1, index: 0, line: 1 }
  else if (String.length o2 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 2, index: 3, line: 1 }
  else if (String.length o3 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 6, index: 7, line: 1 }
  else if (String.length o4 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 10, index: 11, line: 1 }
  else if (String.length o5 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 14, index: 15, line: 1 }
  else if (String.length o6 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 18, index: 19, line: 1 }
  else
    pure $ SixGroupsByColon
      (o1 <> ":" <> o2 <> ":" <> o3 <> ":" <> o4 <> ":" <> o5 <> ":" <> o6)

-- | INTERNAL
-- |
-- | Format: "FF-FF-FF-FF-FF-FF"
pSixGroupsByHyphen :: Parser String Eui48
pSixGroupsByHyphen = do
  o1 <- octet <* char '-'
  o2 <- octet <* char '-'
  o3 <- octet <* char '-'
  o4 <- octet <* char '-'
  o5 <- octet <* char '-'
  o6 <- octet
  eof <?> "end of string"

  if (String.length o1 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 1, index: 0, line: 1 }
  else if (String.length o2 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 2, index: 3, line: 1 }
  else if (String.length o3 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 6, index: 7, line: 1 }
  else if (String.length o4 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 10, index: 11, line: 1 }
  else if (String.length o5 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 14, index: 15, line: 1 }
  else if (String.length o6 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 18, index: 19, line: 1 }
  else
    pure $ SixGroupsByHyphen
      (o1 <> "-" <> o2 <> "-" <> o3 <> "-" <> o4 <> "-" <> o5 <> "-" <> o6)

-- | INTERNAL
-- |
-- | Format: "FFFF.FFFF.FFFF"
pThreeGroupsByDot :: Parser String Eui48
pThreeGroupsByDot = do
  o1 <- octet <* char '.'
  o2 <- octet <* char '.'
  o3 <- octet
  eof <?> "end of string"

  if (String.length o1 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 1, index: 0, line: 1 }
  else if (String.length o2 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 6, index: 5, line: 1 }
  else if (String.length o3 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 11, index: 10, line: 1 }
  else
    pure $ ThreeGroupsByDot (o1 <> "." <> o2 <> "." <> o3)

-- | INTERNAL
pEui64 :: Parser String Eui64
pEui64 = try $ choice
  [ pEightGroupsByColon
  , pEightGroupsByHyphen
  , pFourGroupsByDot
  ]

-- | INTERNAL
-- |
-- | Format: "FF:FF:FF:FF:FF:FF:FF:FF"
pEightGroupsByColon :: Parser String Eui64
pEightGroupsByColon = do
  o1 <- octet <* char ':'
  o2 <- octet <* char ':'
  o3 <- octet <* char ':'
  o4 <- octet <* char ':'
  o5 <- octet <* char ':'
  o6 <- octet <* char ':'
  o7 <- octet <* char ':'
  o8 <- octet
  eof <?> "end of string"
  pure $ EightGroupsByColon
    (o1 <> ":" <> o2 <> ":" <> o3 <> ":" <> o4 <> ":" <> o5 <> ":" <> o6 <> ":" <> o7 <> ":" <> o8)

-- | INTERNAL
-- |
-- | Format: "FF-FF-FF-FF-FF-FF-FF-FF"
pEightGroupsByHyphen :: Parser String Eui64
pEightGroupsByHyphen = do
  o1 <- octet <* char '-'
  o2 <- octet <* char '-'
  o3 <- octet <* char '-'
  o4 <- octet <* char '-'
  o5 <- octet <* char '-'
  o6 <- octet <* char '-'
  o7 <- octet <* char '-'
  o8 <- octet
  eof <?> "end of string"
  pure $ EightGroupsByColon
    (o1 <> "-" <> o2 <> "-" <> o3 <> "-" <> o4 <> "-" <> o5 <> "-" <> o6 <> "-" <> o7 <> "-" <> o8)

-- | INTERNAL
-- |
-- | Format: "FFFF.FFFF.FFFF.FFFF"
pFourGroupsByDot :: Parser String Eui64
pFourGroupsByDot = do
  o1 <- octet <* char '.'
  o2 <- octet <* char '.'
  o3 <- octet <* char '.'
  o4 <- octet
  pure $ FourGroupsByDot
    (o1 <> "." <> o2 <> "." <> o3 <> "." <> o4)

-- | INTERNAL
parser :: Parser String MacAddr
parser = (IPv4 <$> pEui48) <|> (IPv6 <$> pEui64)

-- | INTERNAL
-- |
-- | Give the end user enough information about not only
-- | WHAT went wrong, but also WHERE it wrong.
prettyError :: ParseError -> String
prettyError err = msg <> " starting at position " <> show col
  where
  msg = Parsing.parseErrorMessage err
  Position { column: col, index: _, line: _ } = Parsing.parseErrorPosition err

-- | Parse a string as a possible mac address.
parse_ :: String -> Either String MacAddr
parse_ = lmap prettyError
  <<< flip Parsing.runParser parser
  <<< String.toUpper

-- | Convert ...
upConvert_ :: Eui48 -> Eui64
upConvert_ (SixGroupsByColon mac) = do
  let parts = String.splitAt 9 mac
  EightGroupsByColon (parts.before <> ":FF:FE" <> parts.after)
upConvert_ (SixGroupsByHyphen mac) = do
  let parts = String.splitAt 9 mac
  EightGroupsByHyphen (parts.before <> "-FF-FE" <> parts.after)
upConvert_ (ThreeGroupsByDot mac) = do
  let parts = String.splitAt 7 mac
  FourGroupsByDot (parts.before <> "FF.FE" <> parts.after)
