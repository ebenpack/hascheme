module DataTypes where

import Control.Monad.Except
import Data.Array
import Data.Complex
import Data.IORef
import Data.Map (Map, empty)
import Data.Ratio
import GHC.IO.Handle
import ParserCombinators (ParseError)

type Bindings = IORef (Map String (IORef LispVal))

data Env = Frame
  { parent :: Maybe Env
  , references :: Bindings
  , bindings :: Bindings
  }

type IOThrowsError = ExceptT LispError IO

type PrimitiveFunc = ([LispVal] -> ThrowsError LispVal)

type IOPrimitiveFunc = ([LispVal] -> IOThrowsError LispVal)

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
  = Vector (Array Integer LispVal)
  | Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Integer Integer
  | Rational Rational
  | Float Double
  | Complex (Complex Double)
  | String String
  | Character Char
  | Bool Bool
  | IOFunc String
           IOPrimitiveFunc
  | Port Handle
  | PrimitiveFunc String
                  PrimitiveFunc
  | Func { name :: String
         , params :: [String]
         , vararg :: Maybe String
         , body :: [LispVal]
         , closure :: Env }
  | Pointer { pointerVar :: String
            , pointerEnv :: Env }
  | Void

instance Show LispVal where
  show = showVal

data Arity
  = Min Int
  | MinMax Int
           Int

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name') = name'
showVal (Integer contents) = show contents
showVal (Rational contents) =
  show (numerator contents) ++ "/" ++ show (denominator contents)
showVal (Float contents) = show contents
showVal (Complex contents) =
  show (realPart contents) ++
  (if imagPart contents < 0
     then ""
     else "+") ++
  show (imagPart contents) ++ "i"
showVal (Character contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (Vector contents) = "#(" ++ unwordsList (elems contents) ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc name' _) = "#<procedure:" ++ name' ++ ">"
showVal Func {name = name'} = "#<procedure:" ++ name' ++ ">"
showVal (Port _) = "<IO port>"
showVal (IOFunc name' _) = "#<IO primitive:" ++ name' ++ ">"
showVal Void = ""
showVal (Pointer v e) = "Pointer " ++ v

showError :: LispError -> String
showError (Default message) = message
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found args) =
  let error' =
        case expected of
          Min min' ->
            "arity mismatch;\nthe expected number of arguments does not match the given number" ++
            "\nexpected: at least " ++ show min' ++ "\ngiven: " ++ show found
          MinMax min' max' ->
            "arity mismatch;\nthe expected number of arguments does not match the given number" ++
            "\nexpected: " ++
            (if min' == max'
               then show min'
               else "between " ++ show min' ++ " and " ++ show max') ++
            "\ngiven: " ++ show found
      argsError =
        case args of
          [] -> ""
          a -> "\narguments:\n" ++ unwordsList a
  in error' ++ argsError
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
