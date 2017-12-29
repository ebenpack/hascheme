module Vector where

import Control.Monad.Except
import Data.Array
import DataTypes
       (Arity(..), IOPrimitiveFunc, IOThrowsError, LispError(..),
        LispVal(..), PrimitiveFunc)

vectorPrimitives :: [(String, PrimitiveFunc)]
vectorPrimitives =
  [ ("vector?", isVector)
  , ("vector-length", vectorLength)
  , ("vector-ref", vectorRef)
  ]

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
