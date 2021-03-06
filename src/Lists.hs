module Lists where

import Control.Monad (replicateM)
import Control.Monad.Except
import DataTypes
       (Arity(..), LispError(..), LispVal(..), PrimitiveFunc)

listPrimitives :: [(String, PrimitiveFunc)]
listPrimitives =
  [ ("pair?", isPair)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("empty?", empty)
  ] ++
  accessors

car :: PrimitiveFunc
car [DottedList (x:_) _] = return x
car [List []] = throwError $ Default "Unexpected error in car"
car [List (x:_)] = return $ x
car [badArg] = throwError $ Default $ "car expected pair, found " ++ show badArg
car badArgList =
  throwError $ NumArgs (MinMax 1 1) (length badArgList) badArgList

cdr :: PrimitiveFunc
cdr [DottedList [_] a] = return a
cdr [DottedList (_:xs) a] = return $ DottedList xs a
cdr [List []] = throwError $ Default "cdr on empty list" -- TODO: FIX ERROR MSG
cdr [List (_:[])] = return $ List []
cdr [List (_:a)] = return $ List a
cdr [badArg] = throwError $ Default $ "cdr expected pair, found " ++ show badArg
cdr badArgList =
  throwError $ NumArgs (MinMax 1 1) (length badArgList) badArgList

cons :: PrimitiveFunc
cons [a, List b] = return $ List (a : b)
cons [a, DottedList b c] = return $ DottedList (a : b) c
cons [a, b] = return $ DottedList [a] b
cons badArgList =
  throwError $ NumArgs (MinMax 2 2) (length badArgList) badArgList

isPair :: PrimitiveFunc
isPair [List _] = return $ Bool True
isPair [DottedList _ _] = return $ Bool True
isPair _ = return $ Bool False

empty :: PrimitiveFunc
empty [List []] = return $ Bool True
empty [List _] = return $ Bool False
empty _ = throwError $ Default "Type error: `empty` called on non-list"

accessors :: [(String, PrimitiveFunc)]
accessors =
  map (\a -> ("c" ++ a ++ "r", makeAccessor a)) $
  replicateM 2 ['a', 'd'] ++ replicateM 3 ['a', 'd'] ++ replicateM 4 ['a', 'd']
  where
    makeAccessor :: String -> PrimitiveFunc
    makeAccessor =
      foldr
        (\chr acc ->
           if chr == 'a'
             then comp acc car
             else comp acc cdr)
        identity
    comp :: PrimitiveFunc -> PrimitiveFunc -> PrimitiveFunc
    comp a b c = do
      d <- a c
      b [d]
    identity :: PrimitiveFunc
    identity [n] = return n
    identity a = return $ List a
