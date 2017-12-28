module ParseNumber where

import Data.Char (digitToInt)
import Data.Complex
import Data.Ratio
import DataTypes (LispVal(..))

import Numeric (readDec, readHex, readInt, readOct)
import ParserCombinators
       (Parser(..), (<|>), char, digit, hexDigit, many1, octDigit, oneOf)

readBinary :: ReadS Integer
readBinary = readInt 2 (`elem` "01") digitToInt

parseNumber :: Parser LispVal
parseNumber = parseComplex <|> parseRational <|> parseFloat <|> parseInteger

--------------
-- Integer
--------------
parseInteger :: Parser LispVal
parseInteger =
  parseIntegerDecimal <|> do
    char '#'
    base <- oneOf "bdox"
    parseIntegerBase base

parseIntegerBase :: Char -> Parser LispVal
parseIntegerBase base =
  case base of
    'd' -> parseIntegerDecimal
    'o' -> parseIntegerOctal
    'x' -> parseIntegerHex
    'b' -> parseIntegerBinary

parseIntegerHelper :: ReadS Integer -> Parser Char -> Parser LispVal
parseIntegerHelper reader parser =
  (char '-' >> (parseIntegerHelper' negate)) <|>
  (char '+' >> (parseIntegerHelper' id)) <|>
  (parseIntegerHelper' id)
  where
    parseIntegerHelper' :: (Integer -> Integer) -> Parser LispVal
    parseIntegerHelper' op = Integer . op . fst . head . reader <$> many1 parser

parseIntegerDecimal :: Parser LispVal
parseIntegerDecimal = parseIntegerHelper readDec digit

parseIntegerOctal :: Parser LispVal
parseIntegerOctal = parseIntegerHelper readOct octDigit

parseIntegerHex :: Parser LispVal
parseIntegerHex = parseIntegerHelper readHex hexDigit

parseIntegerBinary :: Parser LispVal
parseIntegerBinary = parseIntegerHelper readBinary (oneOf "01")

--------------
-- Float
--------------
parseFloat :: Parser LispVal
parseFloat =
  parseFloatDecimal <|> do
    char '#'
    base <- oneOf "bdox"
    parseFloatBase base

parseFloatBase :: Char -> Parser LispVal
parseFloatBase base =
  case base of
    'd' -> parseFloatDecimal
    'o' -> parseFloatOctal
    'x' -> parseFloatHex
    'b' -> parseFloatBinary

parseFloatDecimal :: Parser LispVal
parseFloatDecimal = parseFloatHelper 10 digit readDec

parseFloatOctal :: Parser LispVal
parseFloatOctal = parseFloatHelper 8 octDigit readOct

parseFloatBinary :: Parser LispVal
parseFloatBinary = parseFloatHelper 2 (oneOf "01") readBinary

parseFloatHex :: Parser LispVal
parseFloatHex = parseFloatHelper 16 hexDigit readHex

parseFloatHelper :: Int -> Parser Char -> (ReadS Integer) -> Parser LispVal
parseFloatHelper base p reader =
  (char '-' >> (parseFloat' negate)) <|> (char '+' >> parseFloat' id) <|>
  (parseFloat' id)
  where
    helper :: Integer -> Integer -> Double
    helper whole decimal =
      if decimal == 0
        then (fromIntegral whole)
        else let d = fromIntegral decimal :: Double
                 w = fromIntegral whole :: Double
                 b = fromIntegral base :: Double
                 e = logBase b d
                 floored = floor e
                 f = fromIntegral floored
                 g = d / (b ** (f + 1))
             in w + g
    parseFloat' :: (Double -> Double) -> Parser LispVal
    parseFloat' op = do
      w <- many1 p
      char '.'
      d <- many1 p
      let whole = fst . head $ reader w
          decimal = fst . head $ reader d
      return $ Float $ (op (helper whole decimal))

--------------
-- Complex
--------------
parseComplex :: Parser LispVal
parseComplex =
  parseComplexDecimal <|> do
    char '#'
    base <- oneOf "bdox"
    parseComplexBase base

parseComplexBase :: Char -> Parser LispVal
parseComplexBase base =
  case base of
    'd' -> parseComplexDecimal
    'o' -> parseComplexOctal
    'x' -> parseComplexHex
    'b' -> parseComplexBinary

parseComplexDecimal :: Parser LispVal
parseComplexDecimal =
  parseComplexHelper parseIntegerDecimal parseFloatDecimal parseRationalDecimal

parseComplexOctal :: Parser LispVal
parseComplexOctal =
  parseComplexHelper parseIntegerOctal parseFloatOctal parseRationalOctal

parseComplexHex :: Parser LispVal
parseComplexHex =
  parseComplexHelper parseIntegerHex parseFloatHex parseRationalHex

parseComplexBinary :: Parser LispVal
parseComplexBinary =
  parseComplexHelper parseIntegerBinary parseFloatBinary parseRationalBinary

parseComplexHelper ::
     Parser LispVal -> Parser LispVal -> Parser LispVal -> Parser LispVal
parseComplexHelper pn pf pr = do
  real <- fmap toDouble (pr <|> pf <|> pn)
  imaginary <- fmap toDouble (pr <|> pf <|> pn)
  char 'i'
  return $ Complex (real :+ imaginary)
  where
    toDouble (Float x) = x
    toDouble (Integer x) = fromIntegral x
    toDouble (Rational x) = fromRational x

--------------
-- Rational
--------------
parseRational :: Parser LispVal
parseRational =
  parseRationalDecimal <|> do
    char '#'
    base <- oneOf "bdox"
    parseRationalBase base

parseRationalBase :: Char -> Parser LispVal
parseRationalBase base =
  case base of
    'd' -> parseRationalDecimal
    'o' -> parseRationalOctal
    'x' -> parseRationalHex
    'b' -> parseRationalBinary

parseRationalHelper :: Parser LispVal -> Parser LispVal
parseRationalHelper p = do
  num <- fmap toInt p
  char '/'
  denom <- fmap toInt p
  return $ Rational (num % denom)
  where
    toInt (Integer x) = x

parseRationalDecimal :: Parser LispVal
parseRationalDecimal = parseRationalHelper parseIntegerDecimal

parseRationalOctal :: Parser LispVal
parseRationalOctal = parseRationalHelper parseIntegerOctal

parseRationalHex :: Parser LispVal
parseRationalHex = parseRationalHelper parseIntegerHex

parseRationalBinary :: Parser LispVal
parseRationalBinary = parseRationalHelper parseIntegerBinary
