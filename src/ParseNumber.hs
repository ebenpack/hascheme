module ParseNumber where

import Data.Char (digitToInt)
import Data.Complex
import Data.Ratio
import DataTypes (LispVal(..))

import Numeric (readDec, readHex, readInt, readOct)
import ParserCombinators
       (Parser, (<|>), char, digit, hexDigit, many1, octDigit, oneOf,
        skipMany1)

parseNumber :: Parser LispVal
parseNumber = parseComplex <|> parseRational <|> parseFloat <|> parseInteger

parseInteger :: Parser LispVal
parseInteger =
  parseDecimal <|> do
    char '#'
    base <- oneOf "bdox"
    parseIntegerBase base

parseIntegerBase :: Char -> Parser LispVal
parseIntegerBase base =
  case base of
    'd' -> parseDecimal
    'o' -> parseOctal
    'x' -> parseHex
    'b' -> parseBinary

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

parseDecimal :: Parser LispVal
parseDecimal = Number . fst . head . readDec <$> many1 digit

parseOctal :: Parser LispVal
parseOctal = Number . fst . head . readOct <$> many1 octDigit

parseHex :: Parser LispVal
parseHex = Number . fst . head . readHex <$> many1 hexDigit

parseBinary :: Parser LispVal
parseBinary = Number . fst . head . readBinary <$> many1 (oneOf "01")

parseFloatHelper :: Int -> Parser Char -> (ReadS Integer) -> Parser LispVal
parseFloatHelper base p reader = do
  w <- many1 p
  char '.'
  d <- many1 p
  let whole = fst . head $ reader w
      decimal = fst . head $ reader d
  return $ Float (helper whole decimal)
  where
    helper :: Integer -> Integer -> Double
    helper whole decimal =
      if decimal == 0
        then 0
        else let d = fromIntegral decimal :: Double
                 w = fromIntegral whole :: Double
                 b = fromIntegral base :: Double
                 e = logBase b d
                 floored = floor e
                 f = fromIntegral floored
                 g = d / (b ** (f + 1))
             in w + g

parseComplexHelper ::
     Parser LispVal -> Parser LispVal -> Parser LispVal -> Parser LispVal
parseComplexHelper pn pf pr = do
  real <- fmap toDouble (pr <|> pf <|> pn)
  char '+'
  imaginary <- fmap toDouble (pr <|> pf <|> pn)
  char 'i'
  return $ Complex (real :+ imaginary)
  where
    toDouble (Float x) = x
    toDouble (Number x) = fromIntegral x
    toDouble (Rational x) = fromRational x

parseRationalHelper :: Parser LispVal -> Parser LispVal
parseRationalHelper p = do
  num <- fmap toInt p
  char '/'
  denom <- fmap toInt p
  return $ Rational (num % denom)
  where
    toInt (Number x) = x

parseRationalDecimal :: Parser LispVal
parseRationalDecimal = parseRationalHelper parseDecimal

parseRationalOctal :: Parser LispVal
parseRationalOctal = parseRationalHelper parseOctal

parseRationalHex :: Parser LispVal
parseRationalHex = parseRationalHelper parseHex

parseRationalBinary :: Parser LispVal
parseRationalBinary = parseRationalHelper parseBinary

parseComplexDecimal :: Parser LispVal
parseComplexDecimal =
  parseComplexHelper parseDecimal parseFloatDecimal parseRationalDecimal

parseComplexOctal :: Parser LispVal
parseComplexOctal =
  parseComplexHelper parseOctal parseFloatOctal parseRationalOctal

parseComplexHex :: Parser LispVal
parseComplexHex = parseComplexHelper parseHex parseFloatHex parseRationalHex

parseComplexBinary :: Parser LispVal
parseComplexBinary =
  parseComplexHelper parseBinary parseFloatBinary parseRationalBinary

parseFloatDecimal :: Parser LispVal
parseFloatDecimal = parseFloatHelper 10 digit readDec

parseFloatOctal :: Parser LispVal
parseFloatOctal = parseFloatHelper 8 octDigit readOct

parseFloatBinary :: Parser LispVal
parseFloatBinary = parseFloatHelper 2 (oneOf "01") readBinary

parseFloatHex :: Parser LispVal
parseFloatHex = parseFloatHelper 16 hexDigit readHex

readBinary :: ReadS Integer
readBinary = readInt 2 (`elem` "01") digitToInt
