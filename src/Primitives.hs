module Primitives where

import Control.Monad.Except
import Data.Complex
import Data.Ratio
import DataTypes
       (Arity(..), Env, LispError(..), LispVal(..), PrimitiveFunc,
        ThrowsError)

import Lists (listPrimitives)
import Numbers (numPrimitives)
import Strings (strPrimitives)
import Util (boolBinop)

primitives :: [(String, PrimitiveFunc)]
primitives =
  numPrimitives ++
  strPrimitives ++
  listPrimitives ++
  [ ("boolean?", isBoolean)
  , ("symbol?", isSymbol)
  , ("char?", isChar)
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", eqv)
  ]

boolBoolBinop :: (Bool -> Bool -> Bool) -> PrimitiveFunc
boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

isBoolean :: PrimitiveFunc
isBoolean [Bool _] = return $ Bool True
isBoolean _ = return $ Bool False

isSymbol :: PrimitiveFunc
isSymbol [Atom _] = return $ Bool True
isSymbol _ = return $ Bool False

isChar :: PrimitiveFunc
isChar [Character _] = return $ Bool False
isChar _ = return $ Bool False

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left err -> False
        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList =
  throwError $ NumArgs (MinMax 2 2) (length badArgList) badArgList
