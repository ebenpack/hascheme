module Parse where

import Control.Monad (liftM)
import Data.Char (chr, digitToInt, toLower)
import Data.Complex
import Data.Maybe (fromJust, listToMaybe)
import Data.Ratio
import DataTypes (LispVal(..))

import Numeric (readDec, readHex, readInt, readOct)
import ParseNumber (parseNumber)
import ParserCombinators
       (Parser, (<|>), alphanum, char, digit, endBy, letter, many', many1,
        noneOf, oneOf, parse, sepBy, skipMany, skipMany1, space, string,
        try)

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

-- Lists
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  _ <- char '.' >> spaces
  t <- parseExpr
  return $ DottedList h t

parseLists :: Parser LispVal
parseLists = roundBracketList <|> squareBracketList
  where
    roundBracketList = do
      char '('
      x <- parseDottedList <|> parseList
      char ')'
      return x
    squareBracketList = do
      char '['
      x <- parseDottedList <|> parseList
      char ']'
      return x

-- Quoted
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- Backquote
parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  try (char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  try (string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseComment :: Parser LispVal
parseComment = parseLineComment <|> parseBlockComment -- TOD: Fix

parseLineComment :: Parser LispVal
parseLineComment = do
  char ';'
  skipMany $ noneOf "\\n"
  return Void -- TODO: This seems wrong

parseBlockComment :: Parser LispVal
parseBlockComment = do
  string "#|"
  (skipMany $ noneOf "|" >> noneOf "#") -- <|> parseBlockComment
  string "|#"
  return Void -- TODO: This seems wrong

-- TODO: Vector
parseExpr :: Parser LispVal
parseExpr =
  parseNumber <|> parseCharacter <|> parseAtom <|> parseString <|> parseQuoted <|>
  parseLists
