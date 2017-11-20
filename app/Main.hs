module Main where

import Control.Monad (liftM)
import Data.Char (chr, digitToInt, toLower)
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readDec, readHex, readInt, readOct)
import Parse
       (Parser, (<|>), alphanum, char, digit, hexDigit, letter, many',
        many1, noneOf, octDigit, oneOf, parse, skipMany1, space, string)
import System.Environment

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | Float Float
  | String String
  | Character Char
  | Bool Bool
  deriving (Show)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input =
  case parse parseExpr input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many' (escapedChar <|> noneOf "\"")
  char '"'
  return $ String x
  where
    escapedChar :: Parser Char
    escapedChar = (char '\\') >> (oneOf "\"nrt\\")

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

parseCharacter :: Parser LispVal
parseCharacter = do
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

-- parseFloat :: Parser LispVal
-- parseFloat =
--   parseDecimal <|> do
--     char '#'
--     base <- oneOf "bdox"
--     case base of
--       'd' -> parseDecimalFloat
--     --   'o' -> parseOctalFloat
--     --   'x' -> parseHexFloat
--     --   'b' -> parseBinaryFloat
parseNumber :: Parser LispVal
parseNumber =
  parseFloat <|> parseDecimal <|> do
    char '#'
    base <- oneOf "bdox"
    case base of
      'd' -> parseDecimal
      'o' -> parseOctal
      'x' -> parseHex
      'b' -> parseBinary

parseDecimal :: Parser LispVal
parseDecimal = Number . fst . head . readDec <$> many1 digit

parseOctal :: Parser LispVal
parseOctal = Number . fst . head . readOct <$> many1 octDigit

parseHex :: Parser LispVal
parseHex = Number . fst . head . readHex <$> many1 hexDigit

parseBinary :: Parser LispVal
parseBinary = Number . fst . head . readBinary <$> many1 (oneOf "01")

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

parseFloatHelper :: Int -> Parser Char -> (ReadS Integer) -> Parser LispVal
parseFloatHelper base p reader = do
  w <- many1 p
  char '.'
  d <- many1 p
  let whole = fst . head $ reader w
      decimal = fst . head $ reader d
  return $ Float (helper whole decimal)
  where
    helper :: Integer -> Integer -> Float
    helper whole decimal =
      let d = fromIntegral decimal :: Float
          w = fromIntegral whole :: Float
          b = fromIntegral base :: Float
          e = logBase b d
          floored = floor e
          f = fromIntegral floored
          g = d / (b ** (f + 1))
      in w + g

parseFloatDecimal = parseFloatHelper 10 digit readDec

parseFloatOctal = parseFloatHelper 8 octDigit readOct

parseFloatBinary = parseFloatHelper 2 (oneOf "01") readBinary

parseFloatHex = parseFloatHelper 16 hexDigit readHex

-- parseOctalFloat :: Parser LispVal
-- parseOctalFloat = Float . fst . head . readOct <$> many1 octDigit
-- parseHexFloat :: Parser LispVal
-- parseHexFloat = Float . fst . head . readHex <$> many1 hexDigit
-- parseBinaryFloat :: Parser LispVal
-- parseBinaryFloat = do
--   ns <- many1 (oneOf "01")
--   char '.'
--   ms <- many1 (oneOf "01")
--   let n = readBinary ns
--       m = readBinary ms
--       e = floor (logBase 10 m)
--   Float $ (n + (m / e))
--   Number . fromJust . readBinary <$> many1 (oneOf "01")
readBinary :: ReadS Integer
readBinary = readInt 2 (`elem` "01") digitToInt

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString <|> parseCharacter
