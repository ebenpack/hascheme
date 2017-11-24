module Numbers where

import Control.Monad.Except
import Data.Complex
import Data.Ratio
import DataTypes
       (Arity(..), LispError(..), LispVal(..), PrimitiveFunc, ThrowsError)

foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x:xs) = (f v x) >>= \a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x:xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"

numAdd :: PrimitiveFunc
numAdd [] = return $ Number 1
numAdd a = foldlM (\b c -> doAdd =<< (numCast [b, c])) (Number 0) a
  where
    doAdd :: LispVal -> ThrowsError LispVal
    doAdd (List [Number c, Number d]) = return $ Number (c + d)
    doAdd (List [Rational c, Rational d]) = return $ Rational (c + d)
    doAdd (List [Float c, Float d]) = return $ Float (c + d)
    doAdd (List [Complex c, Complex d]) = return $ Complex (c + d)
    doAdd _ = throwError $ Default "Unexpected error in +"

numSub :: PrimitiveFunc
numSub [] = throwError $ NumArgs (Min 1) 0 []
numSub a = foldl1M (\b c -> doSub =<< (numCast [b, c])) a -- TODO zero args check
  where
    doSub :: LispVal -> ThrowsError LispVal
    doSub (List [Number c, Number d]) = return $ Number (c - d)
    doSub (List [Rational c, Rational d]) = return $ Rational (c - d)
    doSub (List [Float c, Float d]) = return $ Float (c - d)
    doSub (List [Complex c, Complex d]) = return $ Complex (c - d)
    doSub _ = throwError $ Default "Unexpected error in -"

numMul :: PrimitiveFunc
numMul [] = return $ Number 1
numMul a = foldl1M (\b c -> doMul =<< (numCast [b, c])) a
  where
    doMul :: LispVal -> ThrowsError LispVal
    doMul (List [Number c, Number d]) = return $ Number (c * d)
    doMul (List [Rational c, Rational d]) = return $ Rational (c * d)
    doMul (List [Float c, Float d]) = return $ Float (c * d)
    doMul (List [Complex c, Complex d]) = return $ Complex (c * d)
    doMul _ = throwError $ Default "Unexpected error in -"

numDiv :: PrimitiveFunc
numDiv [] = throwError $ NumArgs (Min 1) 0 []
numDiv a = foldl1M (\b c -> doDiv =<< (numCast [b, c])) a -- TODO: Zero division error
  where
    doDiv :: LispVal -> ThrowsError LispVal
    doDiv (List [Number c, Number d]) = return $ Rational (c % d)
    doDiv (List [Rational c, Rational d]) = return $ Rational (c / d)
    doDiv (List [Float c, Float d]) = return $ Float (c / d)
    doDiv (List [Complex c, Complex d]) = return $ Complex (c / d)
    doDiv _ = throwError $ Default "Unexpected error in /"

numMod :: PrimitiveFunc
numMod [] = throwError $ NumArgs (MinMax 2 2) 0 []
numMod (a:[]) = throwError $ NumArgs (MinMax 2 2) 1 [a]
numMod a = foldl1M (\b c -> doMod =<< (numCast [b, c])) a -- TODO Arity check
  where
    doMod :: LispVal -> ThrowsError LispVal
    doMod (List [Number c, Number d]) = return $ Number (c `mod` d)
    doMod (List [Rational c, Rational d]) = return $ Rational c -- TODO check if integer-ish, throw error
    doMod (List [Float c, Float d]) = return $ Float c -- TODO check if integer-ish, throw error
    doMod (List [Complex c, Complex d]) = return $ Complex c -- TODO check if integer-ish, throw error
    doMod _ = throwError $ Default "Unexpected error in modulo"

--  (a -> a -> a) -> t a -> a
isInteger :: PrimitiveFunc
isInteger [Number _] = return $ Bool True
isInteger [_] = return $ Bool False
isInteger a = throwError $ NumArgs (MinMax 1 1) (length a) a

isRational :: PrimitiveFunc
isRational [Rational _] = return $ Bool True
isRational a = isInteger a

isReal :: PrimitiveFunc
isReal [Float _] = return $ Bool True
isReal a = isRational a

isComplex :: PrimitiveFunc
isComplex [Complex _] = return $ Bool True
isComplex a = isReal a

isNumber :: PrimitiveFunc
isNumber = isComplex

isBoolean :: PrimitiveFunc
isBoolean [Bool _] = return $ Bool True
isBoolean _ = return $ Bool False

isString :: PrimitiveFunc
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isSymbol :: PrimitiveFunc
isSymbol [Atom _] = return $ Bool True
isSymbol _ = return $ Bool False

isChar :: PrimitiveFunc
isChar [Character _] = return $ Bool False
isChar _ = return $ Bool False

isPair :: PrimitiveFunc
isPair [List _] = return $ Bool True
isPair [DottedList _ _] = return $ Bool True
isPair _ = return $ Bool False

numCast :: [LispVal] -> ThrowsError LispVal
numCast [a@(Number _), b@(Number _)] = return $ List [a, b]
numCast [a@(Rational _), b@(Rational _)] = return $ List [a, b]
numCast [a@(Float _), b@(Float _)] = return $ List [a, b]
numCast [a@(Complex _), b@(Complex _)] = return $ List [a, b]
-- Number
numCast [(Number a), b@(Rational _)] = return $ List [Rational (a % 1), b]
numCast [(Number a), b@(Float _)] = return $ List [Float (fromInteger a), b]
numCast [(Number a), b@(Complex _)] =
  return $ List [Complex (fromInteger a :+ 0), b]
-- Rational
numCast [a@(Rational _), (Number b)] = return $ List [a, Rational (b % 1)]
numCast [(Rational a), b@(Float _)] = return $ List [Float (fromRational a), b]
numCast [(Rational a), b@(Complex _)] =
  return $ List [Complex (fromRational a :+ 0), b]
-- Float
numCast [a@(Float _), (Rational b)] = return $ List [a, Float (fromRational b)]
numCast [a@(Float _), (Number b)] = return $ List [a, Float (fromInteger b)]
numCast [(Float a), b@(Complex _)] = return $ List [Complex (a :+ 0), b]
-- Complex
numCast [a@(Complex _), (Rational b)] =
  return $ List [a, Complex (fromRational b :+ 0)]
numCast [a@(Complex _), (Float b)] = return $ List [a, Complex (b :+ 0)]
numCast [a@(Complex _), (Number b)] =
  return $ List [a, Complex (fromInteger b :+ 0)]
numCast [a, b] =
  case a of
    Number _ -> doThrowError b
    Float _ -> doThrowError b
    Rational _ -> doThrowError b
    Complex _ -> doThrowError b
    _ -> doThrowError a
  where
    doThrowError :: LispVal -> ThrowsError LispVal
    doThrowError num = throwError $ TypeMismatch "number" num
numCast _ = throwError $ Default "Unexpected error in numCast"
