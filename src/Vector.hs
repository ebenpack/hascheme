module Vector where

import Control.Monad.Except
import Data.Array
import DataTypes
       (Arity(..), IOPrimitiveFunc, IOThrowsError, LispError(..),
        LispVal(..), PrimitiveFunc, ThrowsError)

vectorPrimitives :: [(String, PrimitiveFunc)]
vectorPrimitives =
  [ ("vector", vector)
  , ("make-vector", makeVector)
  , ("vector?", isVector)
  , ("vector-length", vectorLength)
  , ("vector-ref", vectorRef)
  ]

makeBounds :: (Integral a) => a -> a -> (Integer, Integer)
makeBounds m n = (fromIntegral m, fromIntegral $ n - 1)

outOfBoundsError :: String -> Integer -> (Array Integer LispVal) -> LispError
outOfBoundsError name index vec =
  Default $
  name ++
  ": index is out of range; " ++
  "index: " ++ show index ++ "; valid range: " ++ show (bounds vec)

isVector :: PrimitiveFunc
isVector args =
  case args of
    [Vector _] -> return $ Bool True
    [_] -> return $ Bool False
    a -> throwError $ NumArgs (MinMax 1 1) (length args) a

vectorLength :: PrimitiveFunc
vectorLength args =
  case args of
    [Vector vec] -> return $ Integer $ fromIntegral $ length vec
    [v] -> throwError $ TypeMismatch "Vector" v
    a -> throwError $ NumArgs (MinMax 1 1) (length args) a

vectorRef :: PrimitiveFunc
vectorRef args =
  case args of
    [Vector vec, Integer n] ->
      if n < (fromIntegral $ length vec)
        then return $ vec ! n
        else throwError $ outOfBoundsError "vector-ref" n vec
    [v] -> throwError $ TypeMismatch "Vector" v
    a -> throwError $ NumArgs (MinMax 2 2) (length args) a

vector :: PrimitiveFunc
vector args =
  let len = (length args) - 1
  in return $ Vector $ listArray (makeBounds 0 (length args)) args

makeVector :: PrimitiveFunc
makeVector args =
  case args of
    [Integer n] -> makeVector' n (Integer 0)
    [Integer n, a] -> makeVector' n a
    [a, _] -> throwError $ TypeMismatch "Integer" a
    a -> throwError $ NumArgs (MinMax 2 2) (length args) a
  where
    makeVector' :: Integer -> LispVal -> ThrowsError LispVal
    makeVector' n v = return $ Vector $ listArray (makeBounds 0 n) $ repeat v
-- vector->list
-- list->vector
-- vector-fill!
-- vector-copy!	
