module DataTypes where

import Control.Monad.Except
import Data.Complex
import Data.Ratio
import ParserCombinators (ParseError)

type PrimitiveFunc = ([LispVal] -> ThrowsError LispVal)

type ThrowsError = Either LispError

data LispError
  = NumArgs Arity
            Int
            [LispVal]
  | TypeMismatch String
                 LispVal
  | Parser ParseError
  | BadSpecialForm String
                   LispVal
  | NotFunction String
                String
  | UnboundVar String
               String
  | Default String

instance Show LispError where
  show = showError

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

data Arity
  = Min Int -- Min
  | MinMax Int -- Min/Max
           Int

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Rational contents) =
  show (numerator contents) ++ "/" ++ show (denominator contents)
showVal (Float contents) = show contents
showVal (Complex contents) =
  show (realPart contents) ++ "+" ++ show (imagPart contents) ++ "i"
showVal (Character contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found args) =
  let error =
        case expected of
          Min min ->
            "arity mismatch;\nthe expected number of arguments does not match the given number" ++
            "\nexpected: at least " ++ show min ++ "\ngiven: " ++ show found
          MinMax min max ->
            "arity mismatch;\nthe expected number of arguments does not match the given number" ++
            "\nexpected: " ++
            (if min == max
               then show min
               else "between " ++ show min ++ " and " ++ show max) ++
            "\ngiven: " ++ show found
      argsError =
        case args of
          [] -> ""
          a -> "\narguments:\n" ++ unwordsList a
  in error ++ argsError
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
