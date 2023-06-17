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
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as Data.String
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.String as Parsing.String
import Parsing.String.Basic as String.Basic

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

-- | INTERNAL
pEui48 :: Parser String Eui48
pEui48 = pSixGroupsByColon
  <|> pSixGroupsByHyphen
  <|> pThreeGroupsByDot

-- | INTERNAL
pSixGroupsByColon :: Parser String Eui48
pSixGroupsByColon = do
  group1 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char ':'
  group2 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char ':'
  group3 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char ':'
  group4 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char ':'
  group5 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  _ <- Parsing.String.char ':'
  group6 <-
    String.Basic.takeWhile1 Unicode.isHexDigit
      <?> "at least 1 hexadecimal char"
  Parsing.String.eof <?> "end of string"

  if (Data.String.length group1 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Parsing.Position { column: 1, index: 0, line: 1 }
  else if (Data.String.length group2 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Parsing.Position { column: 2, index: 3, line: 1 }
  else if (Data.String.length group3 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Parsing.Position { column: 6, index: 7, line: 1 }
  else if (Data.String.length group4 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Parsing.Position { column: 10, index: 11, line: 1 }
  else if (Data.String.length group5 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Parsing.Position { column: 14, index: 15, line: 1 }
  else if (Data.String.length group6 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Parsing.Position { column: 18, index: 19, line: 1 }
  else
    pure $ SixGroupsByColon
      ( intercalate ":"
          [ group1
          , group2
          , group3
          , group4
          , group5
          , group6
          ]
      )

-- | INTERNAL
pSixGroupsByHyphen :: Parser String Eui48
pSixGroupsByHyphen =
  pure $ SixGroupsByHyphen "FF-FF-FF-FF-FF-FF"

-- | INTERNAL
pThreeGroupsByDot :: Parser String Eui48
pThreeGroupsByDot =
  pure $ ThreeGroupsByDot "FFFF.FFFF.FFFF"

-- | INTERNAL
pEui64 :: Parser String Eui64
pEui64 = pEightGroupsByColon
  <|> pEightGroupsByHyphen
  <|> pFourGroupsByDot

-- | INTERNAL
pEightGroupsByColon :: Parser String Eui64
pEightGroupsByColon =
  pure $ EightGroupsByColon "FF:FF:FF:FF:FF:FF:FF:FF"

-- | INTERNAL
pEightGroupsByHyphen :: Parser String Eui64
pEightGroupsByHyphen =
  pure $ EightGroupsByColon "FF-FF-FF-FF-FF-FF-FF-FF"

-- | INTERNAL
pFourGroupsByDot :: Parser String Eui64
pFourGroupsByDot =
  pure $ FourGroupsByDot "FFFF.FFFF.FFFF.FFFF"

-- | INTERNAL
parser :: Parser String MacAddr
parser = (IPv4 <$> pEui48) <|> (IPv6 <$> pEui64)

-- | INTERNAL
prettyError :: Parsing.ParseError -> String
prettyError err = msg <> " starting at position " <> show col
  where
  msg = Parsing.parseErrorMessage err
  Parsing.Position { column: col, index: _, line: _ } = Parsing.parseErrorPosition err

-- | Parse a string as a possible mac address.
parse_ :: String -> Either String MacAddr
parse_ = lmap prettyError
  <<< flip Parsing.runParser parser
  <<< (Data.String.toUpper <<< Data.String.trim)

-- | Convert ...
upConvert_ :: Eui48 -> Eui64
upConvert_ (SixGroupsByColon mac) = do
  let parts = Data.String.splitAt 9 mac
  EightGroupsByColon (parts.before <> ":FF:FE" <> parts.after)
upConvert_ (SixGroupsByHyphen mac) = do
  let parts = Data.String.splitAt 9 mac
  EightGroupsByHyphen (parts.before <> "-FF-FE" <> parts.after)
upConvert_ (ThreeGroupsByDot mac) = do
  let parts = Data.String.splitAt 7 mac
  FourGroupsByDot (parts.before <> "FF.FE" <> parts.after)