module Primitives where

import Control.Monad.Except
import DataTypes
       (Arity(..), IOPrimitiveFunc, IOThrowsError, LispError(..),
        LispVal(..), PrimitiveFunc, ThrowsError)
import Lists (listPrimitives)
import Numbers (numPrimitives)
import Strings (strPrimitives)
import System.IO
import Util (boolBinop)
import Util (liftThrows)
import Vector (vectorPrimitives)

primitives :: [(String, PrimitiveFunc)]
primitives =
  vectorPrimitives ++
  listPrimitives ++
  numPrimitives ++
  strPrimitives ++
  [ ("boolean?", isBoolean)
  , ("symbol?", isSymbol)
  , ("char?", isChar)
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", eqv)
  ]

ioPrimitives :: [(String, IOPrimitiveFunc)]
ioPrimitives =
  [ ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("write", writeProc)
  , ("read-contents", readContents)
  ]

--------------
-- Primitives
--------------
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
eqv [(Integer arg1), (Integer arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2
eqv [(Rational arg1), (Rational arg2)] = return $ Bool $ arg1 == arg2
eqv [(Complex arg1), (Complex arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left _ -> False
        Right (Bool val) -> val
        Right _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList =
  throwError $ NumArgs (MinMax 2 2) (length badArgList) badArgList

--------------
-- IO Primitives
--------------
makePort :: IOMode -> IOPrimitiveFunc
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort _ _ = throwError $ Default "makePort error"

closePort :: IOPrimitiveFunc
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

writeProc :: IOPrimitiveFunc
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc _ = throwError $ Default "writeProc error"

readContents :: IOPrimitiveFunc
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents _ = throwError $ Default "readContents error"
