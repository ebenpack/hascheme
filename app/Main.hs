module Main where

import Control.Monad.Except
import DataTypes
       (LispError(..), LispVal, ThrowsError, extractValue, trapError)
import Eval (eval)
import Parse (parseExpr)
import ParserCombinators (parse)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr input of
    Left (err, s) -> throwError $ Parser err
    Right [(val, _)] -> return val
