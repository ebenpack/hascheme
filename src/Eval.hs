module Eval where

import Control.Monad.Except
import Data.IORef
import DataTypes
       (Arity(..), Env, IOThrowsError, LispError(..), LispVal(..),
        PrimitiveFunc, ThrowsError, showVal)
import Primitives (eqv, primitives)

evalList :: Env -> [LispVal] -> IOThrowsError LispVal
evalList env [] = return Void
evalList env [a] = return a
evalList env (y:ys) = do
  eval env y
  evalList env ys

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Character _) = return val
eval env val@(Number _) = return val
eval env val@(Rational _) = return val
eval env val@(Float _) = return val
eval env val@(Complex _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List ((Atom "cond"):xs)) = evalCond xs
    -- TODO: [test-expr => proc-expr]
  where
    evalCond :: [LispVal] -> IOThrowsError LispVal
    evalCond [] = return Void
    evalCond [List (Atom "else":xs)] = evalList env xs
    evalCond [List (Atom "else":_), _] =
      throwError $ Default "cond: bad syntax (`else` clause must be last)"
    evalCond ((List (pred:conseqs)):xs) = do
      result <- eval env pred
      case result of
        Bool False -> evalCond xs
        _ ->
          case conseqs of
            [] -> return result
            _ -> evalList env conseqs
    evalCond a = throwError $ Default (show a)
eval env (List ((Atom "case"):(key:clauses))) = do
  result <- eval env key
  evalClauses result clauses
  where
    evalClauses :: LispVal -> [LispVal] -> IOThrowsError LispVal
    evalClauses key [List []] = return Void
    evalClauses key (List (datum:exprs):rest) = do
      match <- liftThrows $ inList [key, datum]
      case match of
        Bool True -> evalList env exprs
        Bool False -> evalClauses key rest
    inList :: PrimitiveFunc
    inList [key, List []] = return $ Bool False
    inList [key, List (x:xs)] = do
      eq <- eqv [x, key]
      case eq of
        Bool True -> return $ Bool $ True
        _ -> inList [key, List xs]
    inList [key, badForm] = throwError $ Default "case: bad syntax"
eval env (List ((Atom "case"):_)) =
  throwError $ Default "case: bad syntax in: (case)"
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define":List (Atom var:params):body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params) varargs:body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda":List params:body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda":DottedList params varargs:body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda":varargs@(Atom _):body)) =
  makeVarArgs varargs env [] body
eval env (List (function:args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (Min $ length params) (length args) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>=
         evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
        Nothing -> return env
apply (IOFunc func) args = func args

makeFunc ::
     Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env -- TODO: Move. Here for now just to prevent circular dependency
primitiveBindings =
  nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
