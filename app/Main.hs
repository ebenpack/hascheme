module Main where

import Control.Monad.Except
import DataTypes
       (Env, IOThrowsError, LispError(..), LispVal(..), ThrowsError,
        extractValue, trapError)
import Eval (eval, liftThrows, primitiveBindings, runOne, runRepl)
import Parse (parseExpr)
import qualified ParserCombinators (Parser, endBy, parse, spaces)
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne $ args
