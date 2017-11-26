module Main where

import Control.Monad.Except
import DataTypes
       (Env, IOThrowsError, LispError(..), LispVal, ThrowsError,
        extractValue, trapError)
import Eval (eval, liftThrows, primitiveBindings)
import Parse (parseExpr)
import ParserCombinators (parse)
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    otherwise -> putStrLn "Program takes only 0 or 1 argument"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr input of
    Left (err, _) -> throwError $ Parser err
    Right [(val, _)] -> return val

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl =
  primitiveBindings >>=
  until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
