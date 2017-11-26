module Strings where

import Control.Monad.Except
import Data.Char
import DataTypes
       (Arity(..), LispError(..), LispVal(..), PrimitiveFunc, ThrowsError)
import Util (boolBinop)

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

strBoolBinop :: (String -> String -> Bool) -> PrimitiveFunc
strBoolBinop = boolBinop unpackStr

isString :: PrimitiveFunc
isString [String _] = return $ Bool True
isString _ = return $ Bool False

makeString :: PrimitiveFunc
makeString [n@(Number _)] = makeString [n, Character $ chr 0]
makeString [Number n, Character c] =
  return $ String $ take (fromIntegral n) $ repeat c
makeString _ = throwError $ Default "Invalid arguments to `make-string`"

strLen :: PrimitiveFunc
strLen [String s] = return $ Number $ fromIntegral $ length s
strLen (_:_) = throwError $ Default "Invalid arguments to `string-length`"
strLen _ = throwError $ Default "Invalid arguments to `string-length`"

strPrimitives :: [(String, PrimitiveFunc)]
strPrimitives =
  [ ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("string?", isString)
  , ("make-string", makeString)
  , ("string-length", strLen)
  ]
