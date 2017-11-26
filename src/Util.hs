module Util where

import Control.Monad.Except
import DataTypes
       (Arity(..), LispError(..), LispVal(..), ThrowsError)

boolBinop ::
     (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs (MinMax 2 2) (fromIntegral $ length args) args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right
