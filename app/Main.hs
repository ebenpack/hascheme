module Main where

import DataTypes (LispVal(String))
import Eval (eval)
import Parse (parseExpr)
import ParserCombinators (parse)
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr input of
    Left (err, s) -> DataTypes.String $ "No match: " ++ show err
    Right [(val, s)] -> val
