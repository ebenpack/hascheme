module Primitives where

import Data.Complex
import Data.Ratio
import DataTypes (LispVal(..), PrimitiveFunc)
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
car [List []] = Bool False --TODO: FIX
car [List (x:xs)] = x
car _ = Bool False --TODO: FIX

cdr :: PrimitiveFunc
cdr [DottedList _ a] = a
cdr _ = Bool False --TODO: FIX
