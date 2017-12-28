module Parse where

import Data.Char (chr, toLower)
import DataTypes (LispVal(..))

import ParseNumber (parseNumber)
import ParserCombinators
       (Parser, (<|>), char, digit, endBy, item, letter, many', many1,
        noneOf, oneOf, sepBy, skipMany, skipUntil, spaces, string, try)

--   string "|#"
-- TODO: Vector
parseExpr :: Parser LispVal
parseExpr =
  parseComment <|> parseNumber <|> parseCharacter <|> parseAtom <|> parseString <|>
  parseQuoted <|>
  parseLists

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--------------
-- String
--------------
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many' (escapedChar <|> noneOf "\"\\")
  char '"'
  return $ String x
  where
    escapedChar :: Parser Char
    escapedChar = do
      char '\\'
      x <- oneOf "\\\"nrt"
      return $
        case x of
          '\\' -> x
          '"' -> x
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'

--------------
-- Atom
--------------
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

--------------
-- Char
--------------
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

--------------
-- Lists
--------------
parseList :: Parser LispVal
parseList = do
  skipMany spaces
  list <- sepBy parseExpr spaces
  skipMany spaces
  return $ List list

parseDottedList :: Parser LispVal
parseDottedList = do
  skipMany spaces
  h <- endBy parseExpr spaces
  _ <- char '.' >> spaces
  t <- parseExpr
  skipMany spaces
  return $ DottedList h t

parseLists :: Parser LispVal
parseLists = do
  open <- char '(' <|> char '['
  x <- parseDottedList <|> parseList
  if open == '('
    then char ')'
    else char ']'
  return x

--------------
-- Quoted
--------------
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

--------------
-- Backquote
--------------
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

--------------
-- Comment
--------------
parseComment :: Parser LispVal
parseComment = parseLineComment <|> parseBlockComment -- TODO: Fix

parseLineComment :: Parser LispVal
parseLineComment = do
  char ';'
  skipUntil (char '\n') item
  return Void -- TODO: This seems wrong

parseBlockComment :: Parser LispVal
parseBlockComment = do
  string "#|"
  skipUntil (string "|#") (parseBlockComment <|> takeAnything)
  return Void
  where
    takeAnything = do
      item
      return Void
