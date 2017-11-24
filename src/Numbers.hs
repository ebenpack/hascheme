module Numbers where

import Data.Complex
import Data.Ratio
import DataTypes (LispVal(..), PrimitiveFunc)

numAdd :: PrimitiveFunc
numAdd a = foldl1 (\b c -> doAdd $ (numCast [b, c])) a
  where
    doAdd (List [Number c, Number d]) = Number (c + d)
    doAdd (List [Rational c, Rational d]) = Rational (c + d)
    doAdd (List [Float c, Float d]) = Float (c + d)
    doAdd (List [Complex c, Complex d]) = Complex (c + d)

numSub :: PrimitiveFunc
numSub a = foldl1 (\b c -> doSub $ (numCast [b, c])) a
  where
    doSub (List [Number c, Number d]) = Number (c - d)
    doSub (List [Rational c, Rational d]) = Rational (c - d)
    doSub (List [Float c, Float d]) = Float (c - d)
    doSub (List [Complex c, Complex d]) = Complex (c - d)

numMul :: PrimitiveFunc
numMul a = foldl1 (\b c -> doMul $ (numCast [b, c])) a
  where
    doMul (List [Number c, Number d]) = Number (c * d)
    doMul (List [Rational c, Rational d]) = Rational (c * d)
    doMul (List [Float c, Float d]) = Float (c * d)
    doMul (List [Complex c, Complex d]) = Complex (c * d)

numDiv :: PrimitiveFunc
numDiv a = foldl1 (\b c -> doDiv $ (numCast [b, c])) a -- TODO: Zero division error
  where
    doDiv (List [Number c, Number d]) = Rational (c % d)
    doDiv (List [Rational c, Rational d]) = Rational (c / d)
    doDiv (List [Float c, Float d]) = Float (c / d)
    doDiv (List [Complex c, Complex d]) = Complex (c / d)

numMod :: PrimitiveFunc
numMod a = foldl1 (\b c -> doMod $ (numCast [b, c])) a
  where
    doMod (List [Number c, Number d]) = Number (c `mod` d)
    doMod (List [Rational c, Rational d]) = Rational c -- TODO check if integer-ish, throw error
    doMod (List [Float c, Float d]) = Float c -- TODO check if integer-ish, throw error
    doMod (List [Complex c, Complex d]) = Complex c -- TODO check if integer-ish, throw error

--  (a -> a -> a) -> t a -> a
isInteger :: PrimitiveFunc
isInteger [Number _] = Bool True
isInteger _ = Bool False

isRational :: PrimitiveFunc
isRational [Rational _] = Bool True
isRational a = isInteger a

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

numCast :: [LispVal] -> LispVal
numCast [a@(Number _), b@(Number _)] = List [a, b]
numCast [a@(Rational _), b@(Rational _)] = List [a, b]
numCast [a@(Float _), b@(Float _)] = List [a, b]
numCast [a@(Complex _), b@(Complex _)] = List [a, b]
-- Number
numCast [(Number a), b@(Rational _)] = List [Rational (a % 1), b]
numCast [(Number a), b@(Float _)] = List [Float (fromInteger a), b]
numCast [(Number a), b@(Complex _)] = List [Complex (fromInteger a :+ 0), b]
-- Rational
numCast [a@(Rational _), (Number b)] = List [a, Rational (b % 1)]
numCast [(Rational a), b@(Float _)] = List [Float (fromRational a), b]
numCast [(Rational a), b@(Complex _)] = List [Complex (fromRational a :+ 0), b]
-- Float
numCast [a@(Float _), (Rational b)] = List [a, Float (fromRational b)]
numCast [a@(Float _), (Number b)] = List [a, Float (fromInteger b)]
numCast [(Float a), b@(Complex _)] = List [Complex (a :+ 0), b]
-- Complex
numCast [a@(Complex _), (Rational b)] = List [a, Complex (fromRational b :+ 0)]
numCast [a@(Complex _), (Float b)] = List [a, Complex (b :+ 0)]
numCast [a@(Complex _), (Number b)] = List [a, Complex (fromInteger b :+ 0)]
