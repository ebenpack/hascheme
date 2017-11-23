module DataTypes where

import Data.Complex
import Data.Ratio

type PrimitiveFunc = ([LispVal] -> LispVal)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer -- Integer
  | Rational Rational -- Rational
  | Float Double -- Real
  | Complex (Complex Double) -- Complex
  | String String
  | Character Char
  | Bool Bool
  | PrimitiveFunc PrimitiveFunc

instance Show LispVal where
  show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Rational contents) = show contents
showVal (Float contents) = show contents
showVal (Complex contents) = show contents
showVal (Character contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
