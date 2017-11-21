module Parse where

import Control.Monad (liftM)
import Data.Char (chr, digitToInt, toLower)
import Data.Complex
import Data.Maybe (fromJust, listToMaybe)
import Data.Ratio
import DataTypes (LispVal(..))
import Numeric (readDec, readHex, readInt, readOct)
import ParserCombinators
       (Parser, (<|>), alphanum, char, digit, endBy, hexDigit, letter,
        many', many1, noneOf, octDigit, oneOf, parse, sepBy, skipMany1,
        space, string, try)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- String
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many' (escapedChar <|> noneOf "\"")
  char '"'
  return $ String x
  where
    escapedChar :: Parser Char
    escapedChar = (char '\\') >> (oneOf "\"nrt\\")

-- Atom
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many' (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

-- Char
parseCharacter :: Parser LispVal
parseCharacter
    -- TODO: Meta-, bucky-bit stuff
 = do
  string "#\\"
  c <- many1 letter
  return $
    case map toLower c of
      "newline" -> Character '\n'
      "space" -> Character ' '
      "altmode" -> Character $ chr 27
      "backnext" -> Character $ chr 31
      "backspace" -> Character $ chr 8
      "call" -> Character $ chr 26
      "linefeed" -> Character $ chr 10
      "page" -> Character $ chr 12
      "return" -> Character $ chr 13
      "rubout" -> Character $ chr 127
      "tab" -> Character $ chr 9
      [x] -> Character x

-- Numbers
parseNumber :: Parser LispVal
parseNumber =
  parseDecimal <|> do
    char '#'
    base <- oneOf "bdox"
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

parseComplexHelper :: Parser LispVal -> Parser LispVal -> Parser LispVal
parseComplexHelper pn pf = do
  real <- fmap toDouble (pf <|> pn)
  char '+'
  imaginary <- fmap toDouble (pf <|> pn)
  char 'i'
  return $ Complex (real :+ imaginary)
  where
    toDouble (Float x) = x
    toDouble (Number x) = fromIntegral x

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
parseComplexDecimal = parseComplexHelper parseDecimal parseFloatDecimal

parseComplexOctal :: Parser LispVal
parseComplexOctal = parseComplexHelper parseOctal parseFloatOctal

parseComplexHex :: Parser LispVal
parseComplexHex = parseComplexHelper parseHex parseFloatHex

parseComplexBinary :: Parser LispVal
parseComplexBinary = parseComplexHelper parseBinary parseFloatBinary

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

-- Lists
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseLists :: Parser LispVal
parseLists = do
  char '('
  x <- try $ parseList <|> parseDottedList
  char ')'
  return x

-- Quoted
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseNumber <|> parseAtom <|> parseString <|> parseQuoted <|> parseLists
