module Primitives where

import Data.Complex
import DataTypes (LispVal(..), PrimitiveFunc)

primitives :: [(String, PrimitiveFunc)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop (/))
  -- , ("mod", numericBinop mod)
  -- , ("quotient", numericBinop quot)
  -- , ("remainder", numericBinop rem)
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

car :: PrimitiveFunc
car [List []] = Bool False --TODO: FIX
car [List (x:xs)] = x
car _ = Bool False --TODO: FIX

cdr :: PrimitiveFunc
cdr [DottedList _ a] = a
cdr _ = Bool False --TODO: FIX

isInteger :: PrimitiveFunc
isInteger [Number _] = Bool True
isInteger _ = Bool False

isRational :: PrimitiveFunc
isRational [Rational _] = Bool True
isRational a = isInteger a
isRational _ = Bool False

isReal :: PrimitiveFunc
isReal [Float _] = Bool True
isReal a = isRational a

isComplex :: PrimitiveFunc
isComplex [Complex _] = Bool True
isComplex a = isReal a

isNumber :: PrimitiveFunc
isNumber = isComplex

isBoolean :: PrimitiveFunc
isBoolean [Bool _] = Bool True
isBoolean _ = Bool False

isString :: PrimitiveFunc
isString [String _] = Bool True
isString _ = Bool False

isSymbol :: PrimitiveFunc
isSymbol [Atom _] = Bool True
isSymbol _ = Bool False

isChar :: PrimitiveFunc
isChar [Character _] = Bool False
isChar _ = Bool False

isPair :: PrimitiveFunc
isPair [List _] = Bool True
isPair [DottedList _ _] = Bool True
isPair _ = Bool False

numericBinop ::
     (Complex Double -> Complex Double -> Complex Double) -> PrimitiveFunc
numericBinop op params = Complex $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Complex Double
unpackNum (Number n) = fromInteger n :+ 0
unpackNum (Rational n) = fromRational n :+ 0
unpackNum (Float n) = n :+ 0
unpackNum (Complex v) = v
unpackNum _ = 0
