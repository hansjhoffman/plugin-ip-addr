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
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String
import Parsing (Parser, ParseError, Position(..))
import Parsing as Parsing
import Parsing.Combinators (choice, (<?>))
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

-- | INTERNAL
pEui48 :: Parser String Eui48
pEui48 = choice
  [ pSixGroupsByColon
  , pSixGroupsByHyphen
  , pThreeGroupsByDot
  ]

-- | INTERNAL
-- |
-- | Format: FF:FF:FF:FF:FF:FF
pSixGroupsByColon :: Parser String Eui48
pSixGroupsByColon = do
  group1 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char ':'
  group2 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char ':'
  group3 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char ':'
  group4 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char ':'
  group5 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char ':'
  group6 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  eof <?> "end of string"

  if (String.length group1 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 1, index: 0, line: 1 }
  else if (String.length group2 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 2, index: 3, line: 1 }
  else if (String.length group3 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 6, index: 7, line: 1 }
  else if (String.length group4 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 10, index: 11, line: 1 }
  else if (String.length group5 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 14, index: 15, line: 1 }
  else if (String.length group6 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 18, index: 19, line: 1 }
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
-- |
-- | Format: "FF-FF-FF-FF-FF-FF"
pSixGroupsByHyphen :: Parser String Eui48
pSixGroupsByHyphen = do
  group1 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  group2 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  group3 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  group4 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  group5 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '-'
  group6 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  eof <?> "end of string"

  if (String.length group1 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 1, index: 0, line: 1 }
  else if (String.length group2 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 2, index: 3, line: 1 }
  else if (String.length group3 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 6, index: 7, line: 1 }
  else if (String.length group4 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 10, index: 11, line: 1 }
  else if (String.length group5 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 14, index: 15, line: 1 }
  else if (String.length group6 /= 2) then
    Parsing.failWithPosition "Expected 2 hexadecimal chars" $
      Position { column: 18, index: 19, line: 1 }
  else
    pure $ SixGroupsByHyphen
      ( intercalate "-"
          [ group1
          , group2
          , group3
          , group4
          , group5
          , group6
          ]
      )

-- | INTERNAL
-- |
-- | Format: "FFFF.FFFF.FFFF"
pThreeGroupsByDot :: Parser String Eui48
pThreeGroupsByDot = do
  group1 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '.'
  group2 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  _ <- char '.'
  group3 <- takeWhile1 isHexDigit <?> "at least 1 hexadecimal char"
  eof <?> "end of string"

  if (String.length group1 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 1, index: 0, line: 1 }
  else if (String.length group2 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 6, index: 5, line: 1 }
  else if (String.length group3 /= 4) then
    Parsing.failWithPosition "Expected 4 hexadecimal chars" $
      Position { column: 11, index: 10, line: 1 }
  else
    pure $ ThreeGroupsByDot
      ( intercalate "."
          [ group1
          , group2
          , group3
          ]
      )

-- | INTERNAL
pEui64 :: Parser String Eui64
pEui64 = choice
  [ pEightGroupsByColon
  , pEightGroupsByHyphen
  , pFourGroupsByDot
  ]

-- | INTERNAL
-- |
-- | Format: "FF:FF:FF:FF:FF:FF:FF:FF"
pEightGroupsByColon :: Parser String Eui64
pEightGroupsByColon =
  pure $ EightGroupsByColon "FF:FF:FF:FF:FF:FF:FF:FF"

-- | INTERNAL
-- |
-- | Format: "FF-FF-FF-FF-FF-FF-FF-FF"
pEightGroupsByHyphen :: Parser String Eui64
pEightGroupsByHyphen =
  pure $ EightGroupsByColon "FF-FF-FF-FF-FF-FF-FF-FF"

-- | INTERNAL
-- |
-- | Format: "FFFF.FFFF.FFFF.FFFF"
pFourGroupsByDot :: Parser String Eui64
pFourGroupsByDot =
  pure $ FourGroupsByDot "FFFF.FFFF.FFFF.FFFF"

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
  <<< (String.toUpper <<< String.trim)

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
