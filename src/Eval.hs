module Eval where

import Control.Monad.Except
import Data.Complex
import Data.IORef
import Data.Maybe (isNothing)
import Data.Ratio
import DataTypes
       (Arity(..), Env, EnvFrame(..), IOThrowsError, LispError(..),
        LispVal(..), PrimitiveFunc, ThrowsError, extractValue, showVal,
        trapError)
import Parse
import ParserCombinators
import Paths_hascheme (getDataFileName)
import Primitives (eqv, primitives)
import System.Console.Haskeline
import System.IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

--------------
-- Eval
--------------
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@Void = return val
eval _ val@(String _) = return val
eval _ val@(Character _) = return val
eval _ val@(Integer _) = return val
eval _ (Rational val) =
  if denominator val == 1
    then return $ Integer $ numerator val
    else return $ Rational val
eval _ val@(Float _) = return val
eval _ (Complex val) =
  if imagPart val == 0
    then return $ Float $ realPart val
    else return $ Complex val
eval _ val@(Bool _) = return val
eval env (Atom id') = getVar env id'
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred', conseq, alt]) = do
  result <- eval env pred'
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List ((Atom "cond"):xs)) = evalCond xs
    -- TODO: [test-expr => proc-expr]
  where
    evalCond :: [LispVal] -> IOThrowsError LispVal
    evalCond [] = return Void
    evalCond [List (Atom "else":xs')] = evalList env xs'
    evalCond (List (Atom "else":_):_) =
      throwError $ Default "cond: bad syntax (`else` clause must be last)"
    evalCond ((List (pred':conseqs)):xs') = do
      result <- eval env pred'
      case result of
        Bool False -> evalCond xs'
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
    evalClauses _ [List []] = return Void
    evalClauses key' (List (datum:exprs):rest) = do
      match <- liftThrows $ inList [key', datum]
      case match of
        Bool True -> evalList env exprs
        Bool False -> evalClauses key' rest
        _ -> throwError $ Default "case: bad syntax"
    evalClauses _ _ = throwError $ Default "case: bad syntax"
    inList :: PrimitiveFunc
    inList [_, List []] = return $ Bool False
    inList [key', List (x:xs)] = do
      eq <- eqv [x, key']
      case eq of
        Bool True -> return $ Bool $ True
        _ -> inList [key, List xs]
    inList _ = throwError $ Default "case: bad syntax"
eval _ (List ((Atom "case"):_)) =
  throwError $ Default "case: bad syntax in: (case)"
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define":List (Atom var:params'):body')) =
  makeNormalFunc var env params' body' >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params') varargs:body')) =
  makeVarArgs var varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda":List params':body')) =
  makeNormalFunc "λ" env params' body'
eval env (List (Atom "lambda":varargs@(Atom _):body')) =
  makeVarArgs "λ" varargs env [] body'
eval env (List (Atom "lambda":DottedList params' varargs:body')) =
  makeVarArgs "λ" varargs env params' body'
eval env (List [Atom "let", List pairs, body']) = do
  List atoms <- getHeads pairs
  _ <- mapM_ ensureAtom atoms
  List vals <- getTails pairs
  vals' <- mapM (eval env) vals
  env' <-
    liftIO $
    bindVars env (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals')
  eval env' body'
eval env (List [Atom "let*", List pairs, body']) = do
  List atoms <- getHeads pairs
  _ <- mapM_ ensureAtom atoms
  List vals <- getTails pairs
  env' <- buildEnv env atoms vals
  eval env' body'
  where
    buildEnv :: Env -> [LispVal] -> [LispVal] -> IOThrowsError Env
    buildEnv env' [] [] = return env'
    buildEnv env' (atomH:atomT) (valH:valT) = do
      val <- eval env' valH
      env'' <- liftIO $ bindVars env' [(extractVar atomH, val)]
      buildEnv env'' atomT valT
    buildEnv _ _ _ = throwError $ Default "let*: bad syntax"
eval env (List [Atom "letrec", List pairs, body']) = do
  List atoms <- getHeads pairs
  _ <- mapM_ ensureAtom atoms
  List vals <- getTails pairs
  env' <- liftIO $ bindVars env (map (\a -> (extractVar a, Void)) atoms)
  vals' <- mapM (eval env') vals
  _ <-
    mapM_
      (\(n, v) -> setVar env' n v)
      (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals')
  eval env' body'
eval env (List [Atom "or", expr1, expr2]) = do
  result <- eval env expr1
  case result of
    Bool True -> return result
    _ -> eval env expr2
eval env (List [Atom "and", expr1, expr2]) = do
  result <- eval env expr1
  case result of
    Bool False -> return result
    _ -> eval env expr2
eval env (List [Atom "load", String filename]) = do
  f <- load filename
  _ <- mapM_ (eval env) f
  return Void
eval env (List [Atom "load-print", String filename]) =
  load filename >>= fmap List . mapM (eval env)
eval env (List (function:args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalList :: Env -> [LispVal] -> IOThrowsError LispVal
evalList _ [] = return Void
evalList env [a] = eval env a
evalList env (y:ys) = eval env y >> evalList env ys

getHeads :: [LispVal] -> IOThrowsError LispVal
getHeads [] = return $ List []
getHeads (List (x:_):ys) = do
  List result <- getHeads ys
  return $ List (x : result)
getHeads _ = throwError $ Default "Unexpected error in let"

getTails :: [LispVal] -> IOThrowsError LispVal
getTails [] = return $ List []
getTails (List [_, xs]:ys) = do
  List result <- getTails ys
  return $ List (xs : result)
getTails _ = throwError $ Default "Unexpected error in let"

ensureAtom :: LispVal -> IOThrowsError LispVal
ensureAtom n@(Atom _) = return n
ensureAtom _ = throwError $ Default "Type error"

extractVar :: LispVal -> String
extractVar (Atom atom) = atom

makeFunc ::
     Monad m
  => String
  -> Maybe String
  -> Env
  -> [LispVal]
  -> [LispVal]
  -> m LispVal
makeFunc name' varargs env params' body' =
  return $ Func name' (map showVal params') varargs body' env

makeNormalFunc ::
     String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc name' = makeFunc name' Nothing

makeVarArgs ::
     String -> LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs name' = (makeFunc name') . Just . showVal

isBoundLocal :: EnvFrame -> String -> IO Bool
isBoundLocal envFrame var =
  case lookup var (bindings envFrame) of
    Nothing -> return False
    Just _ -> return True

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  localBound <- isBoundLocal env var
  if localBound
    then return True
    else case env of
           Global _ -> return False
           Frame {parent = parent'} -> isBound parent' var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  envFrame <- liftIO $ readIORef envRef
  case lookup var (bindings envFrame) of
    Just a -> liftIO $ readIORef a
    Nothing ->
      case envFrame of
        Global _ -> throwError $ UnboundVar "Getting an unbound variable" var
        Frame {parent = parent'} -> getVar parent' var

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  envFrame <- liftIO $ readIORef envRef
  case lookup var (bindings envFrame) of
    Just a -> do
      liftIO $ writeIORef a value
      return value
    Nothing ->
      case envFrame of
        Global _ -> throwError $ UnboundVar "Getting an unbound variable" var
        Frame {parent = parent'} -> setVar parent' var value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  envFrame <- liftIO $ readIORef envRef
  alreadyLocallyDefined <- liftIO $ isBoundLocal envFrame var
  if alreadyLocallyDefined
    then throwError $
         Default $ "Duplicate definition for identifier in: " ++ var
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           liftIO $
             writeIORef envRef $
             envFrame {bindings = ((var, valueRef) : (bindings env))}
           return Void

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings' = do
  extended <- extendEnv bindings' envRef
  newIORef extended
  where
    extendEnv :: [(String, LispVal)] -> Env -> IO EnvFrame
    extendEnv bindings'' env = do
      newBindings <- liftIO $ mapM addBinding bindings''
      return $ Frame {parent = env, bindings = newBindings}
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

--------------
-- Apply
--------------
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc _ func) args = liftThrows $ func args
apply (Func _ params' varargs body' closure') args =
  if num params' /= num args && isNothing varargs
    then throwError $ NumArgs (Min $ length params') (length args) args
    else (liftIO $ bindVars closure' $ zip params' args) >>= bindVarArgs varargs >>=
         evalBody
  where
    remainingArgs = drop (length params') args
    num = toInteger . length
    evalBody env = fmap last $ mapM (eval env) body'
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
        Nothing -> return env
apply (IOFunc _ func) args = func args
apply f _ =
  throwError $
  Default $
  "application: not a procedure; " ++
  "expected a procedure that can be applied to arguments; given: " ++ show f

--------------
-- Env
-- Bindings
-- Primitives
--------------
nullEnv :: IO Env
nullEnv = newIORef $ Global {bindings = []}

primitiveBindings :: IO Env -- TODO: Move. Here for now just to prevent circular dependency
primitiveBindings =
  nullEnv >>=
  (flip bindVars $
   map (makeFunc' IOFunc) ioPrimitives ++
   map (makeFunc' PrimitiveFunc) primitives)
  where
    makeFunc' ::
         (String -> ([LispVal] -> a) -> LispVal)
      -> (String, ([LispVal] -> a))
      -> (String, LispVal)
    makeFunc' constructor (var, func) = (var, constructor var func)

loadStdLib :: Env -> IO Env
loadStdLib env = do
  filename <- liftIO $ getDataFileName "lib/stdlib.scm"
  _ <- evalString env $ "(load \"" ++ filename ++ "\")"
  return env

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args
applyProc _ = throwError $ Default "applyProc error"

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort _ _ = throwError $ Default "makePort error"

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc _ = throwError $ Default "readProc error"

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc _ = throwError $ Default "writeProc error"

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents _ = throwError $ Default "readContents error"

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = fmap List $ load filename
readAll _ = throwError $ Default "readAll error"

--------------
-- Run
-- Repl
--------------
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
  case parse parser input of
    Left (err, _) -> throwError $ DataTypes.Parser err
    Right [(val, _)] -> return val
    _ -> throwError $ Default "Read error"

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr (skipMany space))

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ fmap show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: [String] -> IO ()
runOne args = do
  env <-
    primitiveBindings >>=
    flip bindVars [("args", List $ map String $ drop 1 args)]
  _ <- loadStdLib env
  result <-
    runIOThrows $
    fmap show' $ eval env (List [Atom "load-print", String (args !! 0)])
  putStrLn result
  where
    show' :: LispVal -> String
    show' (List contents) = unwordsList' contents
    show' _ = ""
    unwordsList' :: [LispVal] -> String
    unwordsList' = unlines . map showValNewline . filter printable
    showValNewline :: LispVal -> String
    showValNewline Void = ""
    showValNewline v = showVal v
    printable :: LispVal -> Bool
    printable Void = False
    printable _ = True

runRepl :: InputT IO ()
runRepl = do
  env <- lift $ primitiveBindings >>= loadStdLib
  repl env
  where
    repl :: Env -> InputT IO ()
    repl env = do
      minput <- getInputLine "Lisp>>> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          lift $ evalAndPrint env input
          repl env
