module Main where

import Eval (runOne, runRepl)
import System.Console.Haskeline (defaultSettings, runInputT)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runInputT defaultSettings runRepl
    else runOne $ args
