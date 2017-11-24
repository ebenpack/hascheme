module Eval where

import Control.Monad.Except
import DataTypes
       (LispError(..), LispVal(..), PrimitiveFunc, ThrowsError)
import Primitives (primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Rational _) = return val
eval val@(Float _) = return val
eval val@(Complex _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> PrimitiveFunc
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)
