module Strings where

import Control.Monad.Except
import Data.Char
import DataTypes
       (LispError(..), LispVal(..), PrimitiveFunc, ThrowsError)
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
makeString [n@(Integer _)] = makeString [n, Character $ chr 0]
makeString [Integer n, Character c] =
  return $ String $ replicate (fromIntegral n) c
makeString _ = throwError $ Default "Invalid arguments to `make-string`"

strLen :: PrimitiveFunc
strLen [String s] = return $ Integer $ fromIntegral $ length s
strLen (_:_) = throwError $ Default "Invalid arguments to `string-length`"
strLen _ = throwError $ Default "Invalid arguments to `string-length`"

strAppend :: PrimitiveFunc
strAppend [] = return $ String ""
strAppend [s@(String _)] = return s
strAppend ((String s1):(String s2):xs) = strAppend (String (s1 ++ s2) : xs)
strAppend _ = throwError $ Default "Invalid arguments to `string-append`"

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
  , ("string-append", strAppend)
  ]
