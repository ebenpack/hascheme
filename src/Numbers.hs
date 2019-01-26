module Numbers where

import Control.Monad.Except (throwError)
import Data.Complex
import Data.Ratio
import DataTypes
       (Arity(..), LispError(..), LispVal(..), PrimitiveFunc, ThrowsError)

numPrimitives :: [(String, PrimitiveFunc)]
numPrimitives =
  [ ("+", numAdd)
  , ("-", numSub)
  , ("*", numMul)
  , ("/", numDiv)
  , ("modulo", numMod)
  , ("number?", isNumber)
  , ("complex?", isComplex)
  , ("real?", isReal)
  , ("rational?", isRational)
  , ("integer?", isInteger)
  , ("=", numBoolBinopEq)
  , ("/=", numBoolBinopNeq)
  , (">", numBoolBinopGt)
  , ("<", numBoolBinopLt)
  , (">=", numBoolBinopGte)
  , ("<=", numBoolBinopLte)
  , ("quotient", numQuotient)
  , ("remainder", numRem)
  , ("sin", numSine)
  , ("cos", numCos)
  ]

foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x:xs) = f v x >>= \a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x:xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"

numAdd :: PrimitiveFunc
numAdd [] = return $ Integer 0
numAdd a = foldlM (\b c -> doAdd =<< numCast [b, c]) (Integer 0) a
  where
    doAdd :: LispVal -> ThrowsError LispVal
    doAdd (List [Integer c, Integer d]) = return $ Integer (c + d)
    doAdd (List [Rational c, Rational d]) = return $ Rational (c + d)
    doAdd (List [Float c, Float d]) = return $ Float (c + d)
    doAdd (List [Complex c, Complex d]) = return $ Complex (c + d)
    doAdd _ = throwError $ Default "Unexpected error in +"

numSub :: PrimitiveFunc
numSub [] = throwError $ NumArgs (Min 1) 0 []
numSub a = foldl1M (\b c -> doSub =<< numCast [b, c]) a -- TODO zero args check
  where
    doSub :: LispVal -> ThrowsError LispVal
    doSub (List [Integer c, Integer d]) = return $ Integer (c - d)
    doSub (List [Rational c, Rational d]) = return $ Rational (c - d)
    doSub (List [Float c, Float d]) = return $ Float (c - d)
    doSub (List [Complex c, Complex d]) = return $ Complex (c - d)
    doSub _ = throwError $ Default "Unexpected error in -"

numMul :: PrimitiveFunc
numMul [] = return $ Integer 1
numMul a = foldl1M (\b c -> doMul =<< numCast [b, c]) a
  where
    doMul :: LispVal -> ThrowsError LispVal
    doMul (List [Integer c, Integer d]) = return $ Integer (c * d)
    doMul (List [Rational c, Rational d]) = return $ Rational (c * d)
    doMul (List [Float c, Float d]) = return $ Float (c * d)
    doMul (List [Complex c, Complex d]) = return $ Complex (c * d)
    doMul _ = throwError $ Default "Unexpected error in -"

numDiv :: PrimitiveFunc
numDiv [] = throwError $ NumArgs (Min 1) 0 []
numDiv a = foldl1M (\b c -> doDiv =<< numCast [b, c]) a -- TODO: Zero division error
  where
    doDiv :: LispVal -> ThrowsError LispVal
    doDiv (List [Integer c, Integer d]) =
      if d == 0
        then throwError $ Default "Zero division error"
        else return $ Rational (c % d)
    doDiv (List [Rational c, Rational d]) =
      if d == 0
        then throwError $ Default "Zero division error"
        else return $ Rational (c / d)
    doDiv (List [Float c, Float d]) =
      if d == 0.0
        then throwError $ Default "Zero division error"
        else return $ Float (c / d)
    doDiv (List [Complex c, Complex d]) =
      if d == 0
        then throwError $ Default "Zero division error"
        else return $ Complex (c / d)
    doDiv _ = throwError $ Default "Unexpected error in /"

numMod :: PrimitiveFunc
numMod [] = throwError $ NumArgs (MinMax 2 2) 0 []
numMod [a] = throwError $ NumArgs (MinMax 2 2) 1 [a]
numMod [a, b] = do
  c <- numCast [a, b]
  doMod c
  where
    doMod :: LispVal -> ThrowsError LispVal
    doMod (List [Integer c, Integer d]) = return $ Integer (c `mod` d)
    doMod (List [c@(Rational _), d@(Rational _)]) = do
      Integer c' <- numToInt c
      Integer d' <- numToInt d
      return $ Rational ((c' `mod` d') % 1)
    doMod (List [c@(Float _), d@(Float _)]) = do
      Integer c' <- numToInt c
      Integer d' <- numToInt d
      return $ Float (fromInteger (c' `mod` d'))
    doMod (List [c@(Complex _), d@(Complex _)]) = do
      Integer c' <- numToInt c
      Integer d' <- numToInt d
      return $ Complex (fromInteger (c' `mod` d') :+ 0)
    doMod _ = throwError $ Default "Unexpected error in modulo"
numMod a = throwError $ NumArgs (MinMax 2 2) (length a) a

numRem :: PrimitiveFunc
numRem [] = throwError $ NumArgs (MinMax 2 2) 0 []
numRem [a] = throwError $ NumArgs (MinMax 2 2) 1 [a]
numRem [a, b] = do
  c <- numCast [a, b]
  doRem c
  where
    doRem :: LispVal -> ThrowsError LispVal
    doRem (List [Integer c, Integer d]) = return $ Integer (c `rem` d)
    doRem (List [c@(Rational _), d@(Rational _)]) = do
      Integer c' <- numToInt c
      Integer d' <- numToInt d
      return $ Rational ((c' `rem` d') % 1)
    doRem (List [c@(Float _), d@(Float _)]) = do
      Integer c' <- numToInt c
      Integer d' <- numToInt d
      return $ Float (fromInteger (c' `rem` d'))
    doRem (List [c@(Complex _), d@(Complex _)]) = do
      Integer c' <- numToInt c
      Integer d' <- numToInt d
      return $ Complex (fromInteger (c' `rem` d') :+ 0)
    doRem _ = throwError $ Default "Unexpected error in remainder"
numRem a = throwError $ NumArgs (MinMax 2 2) (length a) a

isInteger :: PrimitiveFunc
isInteger [Integer _] = return $ Bool True
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

numToInt :: LispVal -> ThrowsError LispVal
numToInt a@(Integer _) = return a
numToInt (Rational a) =
  if denominator a == 1
    then return $ Integer (numerator a)
    else throwError $ Default "Could not convert rational to integer"
numToInt (Float a) =
  if a == fromInteger (round a)
    then return $ Integer (round a)
    else throwError $ Default "Could not convert float to integer"
numToInt (Complex a) =
  let rp = realPart a
  in if imagPart a == 0 && rp == fromInteger (round rp)
       then return $ Integer (round rp)
       else throwError $ Default "Could not convert complex to integer"
numToInt _ = throwError $ Default "Could not convert non-number to integer"

numCast :: [LispVal] -> ThrowsError LispVal
numCast [a@(Integer _), b@(Integer _)] = return $ List [a, b]
numCast [a@(Rational _), b@(Rational _)] = return $ List [a, b]
numCast [a@(Float _), b@(Float _)] = return $ List [a, b]
numCast [a@(Complex _), b@(Complex _)] = return $ List [a, b]
-- Integer
numCast [Integer a, b@(Rational _)] = return $ List [Rational (a % 1), b]
numCast [Integer a, b@(Float _)] = return $ List [Float (fromInteger a), b]
numCast [Integer a, b@(Complex _)] =
  return $ List [Complex (fromInteger a :+ 0), b]
-- Rational
numCast [a@(Rational _), Integer b] = return $ List [a, Rational (b % 1)]
numCast [Rational a, b@(Float _)] = return $ List [Float (fromRational a), b]
numCast [Rational a, b@(Complex _)] =
  return $ List [Complex (fromRational a :+ 0), b]
-- Float
numCast [a@(Float _), Rational b] = return $ List [a, Float (fromRational b)]
numCast [a@(Float _), Integer b] = return $ List [a, Float (fromInteger b)]
numCast [Float a, b@(Complex _)] = return $ List [Complex (a :+ 0), b]
-- Complex
numCast [a@(Complex _), Rational b] =
  return $ List [a, Complex (fromRational b :+ 0)]
numCast [a@(Complex _), Float b] = return $ List [a, Complex (b :+ 0)]
numCast [a@(Complex _), Integer b] =
  return $ List [a, Complex (fromInteger b :+ 0)]
numCast [a, b] =
  case a of
    Integer _ -> doThrowError b
    Float _ -> doThrowError b
    Rational _ -> doThrowError b
    Complex _ -> doThrowError b
    _ -> doThrowError a
  where
    doThrowError :: LispVal -> ThrowsError LispVal
    doThrowError num = throwError $ TypeMismatch "Integer" num
numCast _ = throwError $ Default "Unexpected error in numCast"

numBoolBinop ::
     String
  -> (LispVal -> LispVal -> ThrowsError LispVal)
  -> LispVal
  -> [LispVal]
  -> ThrowsError LispVal
numBoolBinop name' op b (c:d) = do
  List [b', c'] <- numCast [b, c]
  result <- op b' c'
  case result of
    Bool True -> numBoolBinop name' op c' d
    Bool False -> return $ Bool False
    _ -> throwError $ Default $ "Unexpected error in " ++ name'
numBoolBinop _ _ _ _ = return $ Bool True

numBoolBinopEq :: [LispVal] -> ThrowsError LispVal
numBoolBinopEq [] = throwError $ NumArgs (Min 1) 0 []
numBoolBinopEq (x:xs) = numBoolBinop "=" fn x xs
  where
    fn :: LispVal -> LispVal -> ThrowsError LispVal
    fn (Integer c) (Integer d) = return $ Bool (c == d)
    fn (Rational c) (Rational d) = return $ Bool (c == d)
    fn (Float c) (Float d) = return $ Bool (c == d)
    fn (Complex c) (Complex d) = return $ Bool (c == d)
    fn _ _ = throwError $ Default "Unexpected error in ="

numBoolBinopNeq :: [LispVal] -> ThrowsError LispVal
numBoolBinopNeq [] = throwError $ NumArgs (Min 1) 0 []
numBoolBinopNeq (x:xs) = numBoolBinop "/=" fn x xs
  where
    fn :: LispVal -> LispVal -> ThrowsError LispVal
    fn (Integer c) (Integer d) = return $ Bool (c /= d)
    fn (Rational c) (Rational d) = return $ Bool (c /= d)
    fn (Float c) (Float d) = return $ Bool (c /= d)
    fn (Complex c) (Complex d) = return $ Bool (c /= d)
    fn _ _ = throwError $ Default "Unexpected error in /="

numBoolBinopLt :: [LispVal] -> ThrowsError LispVal
numBoolBinopLt [] = throwError $ NumArgs (Min 1) 0 []
numBoolBinopLt (x:xs) = numBoolBinop "<" fn x xs
  where
    fn :: LispVal -> LispVal -> ThrowsError LispVal
    fn (Integer c) (Integer d) = return $ Bool (c < d)
    fn (Rational c) (Rational d) = return $ Bool (c < d)
    fn (Float c) (Float d) = return $ Bool (c < d)
    fn (Complex _) (Complex _) =
      throwError $ Default "< not defined for complex numbers"
    fn _ _ = throwError $ Default "Unexpected error in <"

numBoolBinopLte :: [LispVal] -> ThrowsError LispVal
numBoolBinopLte [] = throwError $ NumArgs (Min 1) 0 []
numBoolBinopLte (x:xs) = numBoolBinop "<=" fn x xs
  where
    fn :: LispVal -> LispVal -> ThrowsError LispVal
    fn (Integer c) (Integer d) = return $ Bool (c <= d)
    fn (Rational c) (Rational d) = return $ Bool (c <= d)
    fn (Float c) (Float d) = return $ Bool (c <= d)
    fn (Complex _) (Complex _) =
      throwError $ Default "<= not defined for complex numbers"
    fn _ _ = throwError $ Default "Unexpected error in <="

numBoolBinopGt :: [LispVal] -> ThrowsError LispVal
numBoolBinopGt [] = throwError $ NumArgs (Min 1) 0 []
numBoolBinopGt (x:xs) = numBoolBinop ">" fn x xs
  where
    fn :: LispVal -> LispVal -> ThrowsError LispVal
    fn (Integer c) (Integer d) = return $ Bool (c > d)
    fn (Rational c) (Rational d) = return $ Bool (c > d)
    fn (Float c) (Float d) = return $ Bool (c > d)
    fn (Complex _) (Complex _) =
      throwError $ Default "> not defined for complex numbers"
    fn _ _ = throwError $ Default "Unexpected error in >"

numBoolBinopGte :: [LispVal] -> ThrowsError LispVal
numBoolBinopGte [] = throwError $ NumArgs (Min 1) 0 []
numBoolBinopGte (x:xs) = numBoolBinop ">=" fn x xs
  where
    fn :: LispVal -> LispVal -> ThrowsError LispVal
    fn (Integer c) (Integer d) = return $ Bool (c >= d)
    fn (Rational c) (Rational d) = return $ Bool (c >= d)
    fn (Float c) (Float d) = return $ Bool (c >= d)
    fn (Complex _) (Complex _) =
      throwError $ Default ">= not defined for complex numbers"
    fn _ _ = throwError $ Default "Unexpected error in >="

numQuotient :: PrimitiveFunc
numQuotient args =
  if length args /= 2
    then throwError $ NumArgs (MinMax 2 2) (length args) args
    else do
      nums <- numCast args
      case nums of
        List [Integer a, Integer b] -> return $ Integer (a `quot` b)
        _ -> throwError $ Default "Unexpected error in <=" -- TODO better errors

unaryTrig ::
     (Double -> Double) -> (Double -> Double -> Complex Double) -> PrimitiveFunc
unaryTrig op complexOp args =
  if length args /= 1
    then throwError $ NumArgs (MinMax 1 1) (length args) args
    else case args of
           [Integer a] -> return $ Float (op $ fromInteger a)
           [Rational a] -> return $ Float (op $ fromRational a)
           [Float a] -> return $ Float (op a)
           [Complex a] -> return $ Complex $ complexOp (realPart a) (imagPart a)
           _ -> throwError $ Default "Numerical input expected"

numSine :: PrimitiveFunc
numSine = unaryTrig sin (\r i -> sin r * cosh i :+ cos r * sinh i)

numCos :: PrimitiveFunc
numCos =
  unaryTrig cos (\r i -> cos r * cosh i :+ (-1 * sin r * sinh i))
