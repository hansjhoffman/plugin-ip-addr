module Mac
  ( Eui48(..)
  , Eui64(..)
  , MacAddr(..)
  , Octet
  , parse_
  , print_
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Parsing (Parser)
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

type Octet = String

data Eui48
  = SixGroupsByColon Octet Octet Octet Octet Octet Octet
  | SixGroupsByHyphen Octet Octet Octet Octet Octet Octet
  | ThreeGroupsByDot Octet Octet Octet

derive instance eqEui48 :: Eq Eui48
derive instance genericEui48 :: Generic Eui48 _

instance showEui48 :: Show Eui48 where
  show = genericShow

data Eui64
  = EightGroupsByColon Octet Octet Octet Octet Octet Octet Octet Octet
  | EightGroupsByHyphen Octet Octet Octet Octet Octet Octet Octet Octet
  | FourGroupsByDot Octet Octet Octet Octet

derive instance eqEui64 :: Eq Eui64
derive instance genericEui64 :: Generic Eui64 _

instance showEui64 :: Show Eui64 where
  show = genericShow

octet2 :: Parser String Octet
octet2 = (\x y -> fromCharArray [ x, y ]) <$> hexDigit <*> hexDigit

octet4 :: Parser String Octet
octet4 = (\w x y z -> fromCharArray [ w, x, y, z ]) <$> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit

-- | INTERNAL
eui48Parser :: Parser String Eui48
eui48Parser = choice
  [ try pSixGroupsByColon
  , try pSixGroupsByHyphen
  , try pThreeGroupsByDot
  ]
  where
  -- | Format: FF:FF:FF:FF:FF:FF
  pSixGroupsByColon :: Parser String Eui48
  pSixGroupsByColon = do
    o1 <- octet2 <* char ':'
    o2 <- octet2 <* char ':'
    o3 <- octet2 <* char ':'
    o4 <- octet2 <* char ':'
    o5 <- octet2 <* char ':'
    o6 <- octet2
    eof <?> "end of string"
    pure $ SixGroupsByColon o1 o2 o3 o4 o5 o6

  -- | Format: "FF-FF-FF-FF-FF-FF"
  pSixGroupsByHyphen :: Parser String Eui48
  pSixGroupsByHyphen = do
    o1 <- octet2 <* char '-'
    o2 <- octet2 <* char '-'
    o3 <- octet2 <* char '-'
    o4 <- octet2 <* char '-'
    o5 <- octet2 <* char '-'
    o6 <- octet2
    eof <?> "end of string"
    pure $ SixGroupsByHyphen o1 o2 o3 o4 o5 o6

  -- | Format: "FFFF.FFFF.FFFF"
  pThreeGroupsByDot :: Parser String Eui48
  pThreeGroupsByDot = do
    o1 <- octet4 <* char '.'
    o2 <- octet4 <* char '.'
    o3 <- octet4
    eof <?> "end of string"
    pure $ ThreeGroupsByDot o1 o2 o3

-- | INTERNAL
eui64Parser :: Parser String Eui64
eui64Parser = choice
  [ try pEightGroupsByColon
  , try pEightGroupsByHyphen
  , try pFourGroupsByDot
  ]
  where
  -- | Format: "FF:FF:FF:FF:FF:FF:FF:FF"
  pEightGroupsByColon :: Parser String Eui64
  pEightGroupsByColon = do
    o1 <- octet2 <* char ':'
    o2 <- octet2 <* char ':'
    o3 <- octet2 <* char ':'
    o4 <- octet2 <* char ':'
    o5 <- octet2 <* char ':'
    o6 <- octet2 <* char ':'
    o7 <- octet2 <* char ':'
    o8 <- octet2
    eof <?> "end of string"
    pure $ EightGroupsByColon o1 o2 o3 o4 o5 o6 o7 o8

  -- | Format: "FF-FF-FF-FF-FF-FF-FF-FF"
  pEightGroupsByHyphen :: Parser String Eui64
  pEightGroupsByHyphen = do
    o1 <- octet2 <* char '-'
    o2 <- octet2 <* char '-'
    o3 <- octet2 <* char '-'
    o4 <- octet2 <* char '-'
    o5 <- octet2 <* char '-'
    o6 <- octet2 <* char '-'
    o7 <- octet2 <* char '-'
    o8 <- octet2
    eof <?> "end of string"
    pure $ EightGroupsByHyphen o1 o2 o3 o4 o5 o6 o7 o8

  -- | Format: "FFFF.FFFF.FFFF.FFFF"
  pFourGroupsByDot :: Parser String Eui64
  pFourGroupsByDot = do
    o1 <- octet4 <* char '.'
    o2 <- octet4 <* char '.'
    o3 <- octet4 <* char '.'
    o4 <- octet4
    pure $ FourGroupsByDot o1 o2 o3 o4

-- | INTERNAL
parser :: Parser String MacAddr
parser = (IPv4 <$> eui48Parser) <|> (IPv6 <$> eui64Parser)

-- | Parse a string as a possible mac address.
parse_ :: String -> Either String MacAddr
parse_ = lmap Parsing.parseErrorMessage
  <<< flip Parsing.runParser parser
  <<< String.toUpper

print_ :: MacAddr -> String
print_ (IPv4 addr) = case addr of
  (SixGroupsByColon o1 o2 o3 o4 o5 o6) ->
    intercalate ":" [ o1, o2, o3, o4, o5, o6 ]
  (SixGroupsByHyphen o1 o2 o3 o4 o5 o6) ->
    intercalate "-" [ o1, o2, o3, o4, o5, o6 ]
  (ThreeGroupsByDot o1 o2 o3) ->
    intercalate "." [ o1, o2, o3 ]
print_ (IPv6 addr) = case addr of
  (EightGroupsByColon o1 o2 o3 o4 o5 o6 o7 o8) ->
    intercalate ":" [ o1, o2, o3, o4, o5, o6, o7, o8 ]
  (EightGroupsByHyphen o1 o2 o3 o4 o5 o6 o7 o8) ->
    intercalate "-" [ o1, o2, o3, o4, o5, o6, o7, o8 ]
  (FourGroupsByDot o1 o2 o3 o4) ->
    intercalate "." [ o1, o2, o3, o4 ]
