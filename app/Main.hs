module Main where

import Parse (parseExpr)
import ParserCombinators (parse)
import System.Environment

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input =
  case parse parseExpr input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val
