{-# LANGUAGE LambdaCase #-}

module ParserCombinators
  ( ParseError(..)
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
  , spaces
  , char
  , string
  , many'
  , many1
  , skipMany
  , skipMany1
  , skipUntil
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

newtype ParseError =
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
        _ -> Left (ParseError "Error", [])

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
      (_:_) -> Left (ParseError s, s1)

item :: Parser Char
item =
  Parser $ \case
      [] -> Left (ParseError "'Item' run on empty input", [])
      (x:xs) -> Right [(x, xs)]

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
  Parser $ \inp ->
    case parse p inp of
      Left _ -> parse q inp
      Right [(v, out)] -> Right [(v, out)]
      _ -> Left (ParseError "Error", [])

many' :: Parser a -> Parser [a]
many' p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many' p
  return (v : vs)

skipMany :: Parser a -> Parser ()
skipMany p = do
  _ <- many' p
  return ()

skipMany1 :: Parser a -> Parser ()
skipMany1 p = do
  _ <- many1 p
  return ()

skipUntil :: Parser t -> Parser a -> Parser ()
skipUntil end p = scan
  where
    scan =
      do _ <- end
         return ()
     <|> do
        _ <- p
        scan
        return ()

oneOf :: String -> Parser Char
oneOf [] = failure "Empty input to 'OneOf'"
oneOf (x:xs) = char x <|> oneOf xs

noneOf :: String -> Parser Char
noneOf [] = item
noneOf (x:_) = rej (== x)

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
    _ <- sep
    return x

try :: Parser a -> Parser a
try p =
  Parser $ \s ->
    case parse p s of
      Left (err, _) -> Left (err, s)
      Right [(a, s1)] -> Right [(a, s1)]
      _ -> Left (ParseError "Error", [])

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

spaces :: Parser ()
spaces = skipMany1 space

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)
