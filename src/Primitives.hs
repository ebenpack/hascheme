module Primitives where

import Control.Monad.Except
import Data.Complex
import Data.Ratio
import DataTypes (LispError(..), LispVal(..), PrimitiveFunc)
import Numbers

primitives :: [(String, PrimitiveFunc)]
primitives =
  [ ("+", numAdd)
  , ("-", numSub)
  , ("*", numMul)
  , ("/", numDiv)
  , ("modulo", numMod) -- TODO arity
  , ("number?", isNumber)
  , ("complex?", isComplex)
  , ("real?", isReal)
  , ("rational?", isRational)
  , ("integer?", isInteger)
  , ("boolean?", isBoolean)
  , ("string?", isString)
  , ("symbol?", isSymbol)
  , ("char?", isChar)
  , ("pair?", isPair)
  , ("car", car)
  ]

--   , ("quotient", numericBinop quot)
--   , ("remainder", numericBinop rem)
car :: PrimitiveFunc
car [List []] = throwError $ Default "Unexpected error in car"
car [List (x:xs)] = return $ x
car _ = throwError $ Default "Unexpected error in cdr"

cdr :: PrimitiveFunc
cdr [DottedList _ a] = return a
cdr _ = throwError $ Default "Unexpected error in +"
