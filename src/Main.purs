-- Credit to https://github.com/purescript-contrib/purescript-uri/blob/v7.0.0/src/URI/Host/IPv4Address.purs

module Main
  ( IPv4(..)
  , parse_
  , print_
  , unsafeFromInts
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, singleton)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators (choice, try, (<?>))
import Parsing.String (char, eof, satisfy)
import Parsing.String.Basic (digit)
import Partial.Unsafe (unsafeCrashWith)

data IPv4 = IPv4 Int Int Int Int

derive instance eqIPv4 :: Eq IPv4
derive instance orIPv4 :: Ord IPv4

instance showIPv4 :: Show IPv4 where
  show (IPv4 o1 o2 o3 o4) = "(IPv4 " <> show o1 <> " " <> show o2 <> " " <> show o3 <> " " <> show o4 <> ")"

-- | INTERNAL
-- |
-- | A parser for an IPv4 address according to the spec:
-- | https://www.rfc-editor.org/rfc/rfc791
parser :: Parser String IPv4
parser = do
  o1 <- octet <* char '.'
  o2 <- octet <* char '.'
  o3 <- octet <* char '.'
  o4 <- octet
  eof <?> "end of string"
  pure $ IPv4 o1 o2 o3 o4

octet :: Parser String Int
octet = toInt =<<
  ( choice
      [ try ((\x y z -> fromCharArray [ x, y, z ]) <$> nzDigit <*> digit <*> digit)
      , try ((\x y -> fromCharArray [ x, y ]) <$> nzDigit <*> digit)
      , (singleton <$> digit)
      ]
  ) <?> "octet to be 1-3 digit(s)"

-- | Parsers a non-zero digit
nzDigit :: Parser String Char
nzDigit = satisfy (\c -> c >= '1' && c <= '9')

toInt ∷ String → Parser String Int
toInt s = case Int.fromString s of
  Just n | n >= 0 && n <= 255 → Parsing.liftEither $ Right n
  _ → Parsing.liftEither $ Left "IPv4 octet out of range: 0 >= n <= 255"

-- | INTERNAL
-- |
-- | Constructs an `IPv4` address safely by bounds checking each
-- | octet to ensure it is within the range 0-255 (inclusive).
fromInts :: Int -> Int -> Int -> Int -> Maybe IPv4
fromInts o1 o2 o3 o4 =
  IPv4 <$> check o1 <*> check o2 <*> check o3 <*> check o4
  where
  check ∷ Int → Maybe Int
  check i
    | i >= 0 && i <= 255 = Just i
    | otherwise = Nothing

-- | Constructs an `IPv4` address UNSAFELY: if any of the octets are
-- | outside the allowable bounds, a runtime error will be thrown.
-- |
-- | This is intended as a convenience when describing `IPv4` addresses
-- | statically in PureScript code, in all other cases `fromInts` should be
-- | used.
unsafeFromInts ∷ Int → Int → Int → Int → IPv4
unsafeFromInts o1 o2 o3 o4 =
  case fromInts o1 o2 o3 o4 of
    Just addr → addr
    Nothing → unsafeCrashWith "IPv4 octet was out of range"

-- | Parse a string as a possible `IPv4` address.
parse_ :: String -> Either String IPv4
parse_ = lmap Parsing.parseErrorMessage
  <<< flip Parsing.runParser parser

print_ :: IPv4 -> String
print_ (IPv4 o1 o2 o3 o4) =
  show o1 <> "." <> show o2 <> "." <> show o3 <> "." <> show o4
