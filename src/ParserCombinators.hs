module ParserCombinators
  ( ParseError
  , ParseResult
  , Parser
  , parse
  , failure
  , item
  , return
  , (<|>)
  , sat
  , rej
  , digit
  , lower
  , upper
  , letter
  , alphanum
  , space
  , char
  , string
  , many'
  , many1
  , skipMany
  , skipMany1
  , oneOf
  , noneOf
  , hexDigit
  , octDigit
  ) where

import Data.Char
       (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)

data ParseError =
  ParseError String
  deriving (Show)

type ParseResult a = Either ParseError [(a, String)]

newtype Parser a = Parser
  { parse :: String -> ParseResult a
  }

instance Monad Parser where
  return v = Parser $ \inp -> Right [(v, inp)]
  p >>= f =
    Parser $ \inp ->
      case parse p inp of
        Left a -> Left a
        Right [(v, out)] -> parse (f v) out

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

failure :: Parser a
failure = Parser $ \_ -> Left (ParseError "Error")

item :: Parser Char
item =
  Parser $ \inp ->
    case inp of
      [] -> Left (ParseError "Error")
      (x:xs) -> Right [(x, xs)]

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
  Parser $ \inp ->
    case parse p inp of
      Left _ -> parse q inp
      Right [(v, out)] -> Right [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p =
  item >>= \x ->
    if p x
      then return x
      else failure

rej :: (Char -> Bool) -> Parser Char
rej p =
  item >>= \x ->
    if not (p x)
      then return x
      else failure

digit :: Parser Char
digit = sat isDigit

hexDigit :: Parser Char
hexDigit = digit <|> oneOf "ABCDEFabcdef"

octDigit :: Parser Char
octDigit = oneOf "01234567"

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

space :: Parser Char
space = sat isSpace

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x : xs)

many' :: Parser a -> Parser [a]
many' p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many' p
  return (v : vs)

skipMany :: Parser a -> Parser ()
skipMany p = do
  many' p
  return ()

skipMany1 :: Parser a -> Parser ()
skipMany1 p = do
  many1 p
  return ()

oneOf :: String -> Parser Char
oneOf [] = failure
oneOf (x:xs) = do
  y <- char x <|> oneOf xs
  return y

noneOf :: String -> Parser Char
noneOf [] = item
noneOf (x:xs) = do
  y <- notChar x
  return y
  where
    notChar c = rej (== c)
