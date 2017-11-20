module DataTypes where

import Data.Complex
import Data.Ratio

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | Float Double
  | Rational Rational
  | Complex (Complex Double)
  | String String
  | Character Char
  | Bool Bool
  deriving (Show)
