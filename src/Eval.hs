module Eval where

import DataTypes (LispVal(..), PrimitiveFunc)
import Primitives (primitives)

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Character _) = val
eval val@(Number _) = val
eval val@(Rational _) = val
eval val@(Float _) = val
eval val@(Complex _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args

apply :: String -> PrimitiveFunc
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
