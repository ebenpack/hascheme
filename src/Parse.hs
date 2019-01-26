module Parse where

import Data.Array
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
  parseVector <|> parseComment <|> parseNumber <|> parseCharacter <|> parseAtom <|>
  parseString <|>
  parseQuoted <|>
  parseLists

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--------------
-- String
--------------
parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many' (escapedChar <|> noneOf "\"\\")
  _ <- char '"'
  return $ String x
  where
    escapedChar :: Parser Char
    escapedChar = do
      _ <- char '\\'
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
  _ <- string "#\\"
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
  _ <- skipMany spaces
  list <- sepBy parseExpr spaces
  _ <- skipMany spaces
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
  _ <-
    if open == '('
      then char ')'
      else char ']'
  return x

--------------
-- Vector
--------------
parseVector :: Parser LispVal
parseVector = do
  _ <- char '#'
  open <- char '(' <|> char '['
  List list <- parseList
  _ <-
    if open == '('
      then char ')'
      else char ']'
  let len = length list - 1
  return $ Vector $ listArray (0, fromIntegral len) list

--------------
-- Quoted
--------------
parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

--------------
-- Backquote
--------------
parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  _ <- try (char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  _ <- try (string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

--------------
-- Comment
--------------
parseComment :: Parser LispVal
parseComment = parseLineComment <|> parseBlockComment -- TODO: Fix

parseLineComment :: Parser LispVal
parseLineComment = do
  _ <- char ';'
  _ <- skipUntil (char '\n') item
  return Void -- TODO: This seems wrong

parseBlockComment :: Parser LispVal
parseBlockComment = do
  _ <- string "#|"
  _ <- skipUntil (string "|#") (parseBlockComment <|> takeAnything)
  return Void
  where
    takeAnything = do
      _ <- item
      return Void
