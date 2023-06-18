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
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Parsing (Parser, ParseError, Position(..))
import Parsing as Parsing
import Parsing.Combinators (choice, try, (<?>))
import Parsing.String (char, eof)
import Parsing.String.Basic (hexDigit)

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

octet2 :: Parser String String
octet2 = (\x y -> fromCharArray [ x, y ]) <$> hexDigit <*> hexDigit <?> "octet to be 2 digits"

octet4 :: Parser String String
octet4 = (\w x y z -> fromCharArray [ w, x, y, z ]) <$> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit <?> "octet to be 4 digits"

-- | INTERNAL
eui48Parser :: Parser String Eui48
eui48Parser = choice
  [ try sixGroupsByColon
  , try sixGroupsByHyphen
  , try threeGroupsByDot
  ]
  where
  -- | Format: FF:FF:FF:FF:FF:FF
  sixGroupsByColon :: Parser String Eui48
  sixGroupsByColon = do
    o1 <- octet2 <* char ':'
    o2 <- octet2 <* char ':'
    o3 <- octet2 <* char ':'
    o4 <- octet2 <* char ':'
    o5 <- octet2 <* char ':'
    o6 <- octet2
    eof <?> "end of string"
    pure $ SixGroupsByColon
      (o1 <> ":" <> o2 <> ":" <> o3 <> ":" <> o4 <> ":" <> o5 <> ":" <> o6)

  -- | Format: "FF-FF-FF-FF-FF-FF"
  sixGroupsByHyphen :: Parser String Eui48
  sixGroupsByHyphen = do
    o1 <- octet2 <* char '-'
    o2 <- octet2 <* char '-'
    o3 <- octet2 <* char '-'
    o4 <- octet2 <* char '-'
    o5 <- octet2 <* char '-'
    o6 <- octet2
    eof <?> "end of string"
    pure $ SixGroupsByHyphen
      (o1 <> "-" <> o2 <> "-" <> o3 <> "-" <> o4 <> "-" <> o5 <> "-" <> o6)

  -- | Format: "FFFF.FFFF.FFFF"
  threeGroupsByDot :: Parser String Eui48
  threeGroupsByDot = do
    o1 <- octet4 <* char '.'
    o2 <- octet4 <* char '.'
    o3 <- octet4
    eof <?> "end of string"
    pure $ ThreeGroupsByDot (o1 <> "." <> o2 <> "." <> o3)

-- | INTERNAL
eui64Parser :: Parser String Eui64
eui64Parser = choice
  [ try eightGroupsByColon
  , try eightGroupsByHyphen
  , try fourGroupsByDot
  ]
  where
  -- | Format: "FF:FF:FF:FF:FF:FF:FF:FF"
  eightGroupsByColon :: Parser String Eui64
  eightGroupsByColon = do
    o1 <- octet2 <* char ':'
    o2 <- octet2 <* char ':'
    o3 <- octet2 <* char ':'
    o4 <- octet2 <* char ':'
    o5 <- octet2 <* char ':'
    o6 <- octet2 <* char ':'
    o7 <- octet2 <* char ':'
    o8 <- octet2
    eof <?> "end of string"
    pure $ EightGroupsByColon
      (o1 <> ":" <> o2 <> ":" <> o3 <> ":" <> o4 <> ":" <> o5 <> ":" <> o6 <> ":" <> o7 <> ":" <> o8)

  -- | Format: "FF-FF-FF-FF-FF-FF-FF-FF"
  eightGroupsByHyphen :: Parser String Eui64
  eightGroupsByHyphen = do
    o1 <- octet2 <* char '-'
    o2 <- octet2 <* char '-'
    o3 <- octet2 <* char '-'
    o4 <- octet2 <* char '-'
    o5 <- octet2 <* char '-'
    o6 <- octet2 <* char '-'
    o7 <- octet2 <* char '-'
    o8 <- octet2
    eof <?> "end of string"
    pure $ EightGroupsByColon
      (o1 <> "-" <> o2 <> "-" <> o3 <> "-" <> o4 <> "-" <> o5 <> "-" <> o6 <> "-" <> o7 <> "-" <> o8)

  -- | Format: "FFFF.FFFF.FFFF.FFFF"
  fourGroupsByDot :: Parser String Eui64
  fourGroupsByDot = do
    o1 <- octet4 <* char '.'
    o2 <- octet4 <* char '.'
    o3 <- octet4 <* char '.'
    o4 <- octet4
    pure $ FourGroupsByDot
      (o1 <> "." <> o2 <> "." <> o3 <> "." <> o4)

-- | INTERNAL
parser :: Parser String MacAddr
parser = (IPv4 <$> eui48Parser) <|> (IPv6 <$> eui64Parser)

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
