module ParserCombinators
  ( ParseError(..)
  , ParseResult(..)
  , Parser(..)
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
  , sepBy
  , endBy
  , oneOf
  , noneOf
  , try
  , hexDigit
  , octDigit
  ) where

import Data.Char
       (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)

data ParseError =
  ParseError String
  deriving (Show, Eq)

type ParseResult a = Either (ParseError, String) [(a, String)]

newtype Parser a = Parser
  { parse :: String -> ParseResult a
  }

instance Monad Parser where
  return v = Parser $ \inp -> Right [(v, inp)]
  p >>= f =
    Parser $ \inp ->
      case parse p inp of
        Left (err, _) -> Left (err, inp)
        Right [(v, out)] -> parse (f v) out

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

failure :: String -> Parser a
failure s =
  Parser $ \s1 ->
    case s1 of
      [] -> Left (ParseError s, [])
      (x:xs) -> Left (ParseError s, s1)

item :: Parser Char
item =
  Parser $ \inp ->
    case inp of
      [] -> Left (ParseError "'Item' run on empty input", [])
      (x:xs) -> Right [(x, xs)]

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
  Parser $ \inp ->
    case parse p inp of
      Left _ -> parse q inp
      Right [(v, out)] -> Right [(v, out)]

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
oneOf [] = failure "Empty input to 'OneOf'"
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

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep =
  (do x <- p
      xs <- many' (sep >> p)
      return (x : xs)) <|>
  return []

endBy :: Parser a -> Parser b -> Parser [a]
endBy p sep =
  many' $ do
    x <- p
    sep
    return x

try :: Parser a -> Parser a
try p =
  Parser $ \s ->
    case parse p s of
      Left (err, _) -> Left (err, s)
      Right [(a, s1)] -> Right [(a, s1)]

sat :: (Char -> Bool) -> Parser Char
sat p =
  try $ do
    x <- item
    if p x
      then return x
      else failure $ "Condition not satisfied for: `" ++ [x] ++ "`"

rej :: (Char -> Bool) -> Parser Char
rej p = do
  x <- item
  if not (p x)
    then return x
    else failure $ "Rejection condition not satisfied for: `" ++ [x] ++ "`"

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
