module Eval where

import           Control.Monad.Except
import           Data.Array
import           Data.Complex
import           Data.IORef
import           Data.Map                 (empty, fromList, insert, keys,
                                           lookup)
import           Data.Maybe               (isNothing)
import           Data.Ratio
import           DataTypes                (Arity (..), Env (..),
                                           IOPrimitiveFunc, IOThrowsError,
                                           LispError (..), LispVal (..),
                                           PrimitiveFunc, ThrowsError,
                                           extractValue, showVal, trapError)
import           Parse
import           ParserCombinators
import           Paths_hascheme           (getDataFileName)
import           Primitives               (eqv, ioPrimitives, primitives)
import           System.Console.Haskeline
import           System.IO
import           Util                     (liftThrows)
import           Vector                   (outOfBoundsError)

--------------
-- Eval
--------------
evalLisp :: Env -> LispVal -> IOThrowsError LispVal
evalLisp env lisp = do
    v <- eval env lisp
    derefPtrs [] v

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@Void            = return val
eval _ val@(Pointer _ _  ) = return val
eval _ val@(String    _  ) = return val
eval _ val@(Character _  ) = return val
eval _ val@(Integer   _  ) = return val
eval _ val@(Vector    _  ) = return val
eval _ (    Rational  val) = if denominator val == 1
    then return $ Integer $ numerator val
    else return $ Rational val
eval _ val@(Float   _  ) = return val
eval _ (    Complex val) = if imagPart val == 0
    then return $ Float $ realPart val
    else return $ Complex val
eval _   val@(Bool _) = return val
eval env (    Atom a) = do
    v <- getVar env a
    return $ case v of
        List _         -> Pointer a env
        DottedList _ _ -> Pointer a env
        String _       -> Pointer a env
        Vector _       -> Pointer a env
        _              -> v
eval _   (List [Atom "quote", val]            ) = return val
eval env (List [Atom "if", pred', conseq, alt]) = do
    result <- eval env pred'
    case result of
        Bool False -> eval env alt
        _          -> eval env conseq
eval env (List (Atom "cond" : xs)) = evalCond xs
    -- TODO: [test-expr => proc-expr]
  where
    evalCond :: IOPrimitiveFunc
    evalCond []                         = return Void
    evalCond [List (Atom "else" : xs')] = evalList env xs'
    evalCond (List (Atom "else" : _) : _) =
        throwError $ Default "cond: bad syntax (`else` clause must be last)"
    evalCond (List (pred' : conseqs) : xs') = do
        result <- eval env pred'
        case result of
            Bool False -> evalCond xs'
            _          -> case conseqs of
                [] -> return result
                _  -> evalList env conseqs
    evalCond a = throwError $ Default (show a)
eval env (List (Atom "case" : (key : clauses))) = do
    result <- eval env key
    evalClauses result clauses
  where
    evalClauses :: LispVal -> IOPrimitiveFunc
    evalClauses _    [List []                    ] = return Void
    evalClauses key' (List (datum : exprs) : rest) = do
        match <- liftThrows $ inList [key', datum]
        case match of
            Bool True  -> evalList env exprs
            Bool False -> evalClauses key' rest
            _          -> throwError $ Default "case: bad syntax"
    evalClauses _ _ = throwError $ Default "case: bad syntax"
    inList :: PrimitiveFunc
    inList [_   , List []      ] = return $ Bool False
    inList [key', List (x : xs)] = do
        eq <- eqv [x, key']
        case eq of
            Bool True -> return $ Bool True
            _         -> inList [key, List xs]
    inList _ = throwError $ Default "case: bad syntax"
eval _ (List (Atom "case" : _)) =
    throwError $ Default "case: bad syntax in: (case)"
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) =
    makeNormalFunc var env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body'))
    = makeVarArgs var varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
    makeNormalFunc "λ" env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
    makeVarArgs "λ" varargs env [] body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
    makeVarArgs "λ" varargs env params' body'
eval env (List [Atom "let", List pairs, body']) = do
    List atoms <- getHeads pairs
    _          <- mapM_ ensureAtom atoms
    List vals  <- getTails pairs
    vals'      <- mapM (eval env) vals
    env'       <- liftIO
        $ bindVars env (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals')
    eval env' body'
eval env (List [Atom "let*", List pairs, body']) = do
    List atoms <- getHeads pairs
    _          <- mapM_ ensureAtom atoms
    List vals  <- getTails pairs
    env'       <- buildEnv env atoms vals
    eval env' body'
  where
    buildEnv :: Env -> [LispVal] -> [LispVal] -> IOThrowsError Env
    buildEnv env' []              []            = return env'
    buildEnv env' (atomH : atomT) (valH : valT) = do
        val   <- eval env' valH
        env'' <- liftIO $ bindVars env' [(extractVar atomH, val)]
        buildEnv env'' atomT valT
    buildEnv _ _ _ = throwError $ Default "let*: bad syntax"
eval env (List [Atom "letrec", List pairs, body']) = do
    List atoms <- getHeads pairs
    _          <- mapM_ ensureAtom atoms
    List vals  <- getTails pairs
    env'       <- liftIO $ bindVars env (map (\a -> (extractVar a, Void)) atoms)
    vals'      <- mapM (eval env') vals
    _ <- mapM_ (uncurry (setVar env'))
               (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals')
    eval env' body'
eval env (List [Atom "or", expr1, expr2]) = do
    result <- eval env expr1
    case result of
        Bool True -> return result
        _         -> eval env expr2
eval env (List [Atom "and", expr1, expr2]) = do
    result <- eval env expr1
    case result of
        Bool False -> return result
        _          -> eval env expr2
eval env (List [Atom "load", String filename]) = do
    f <- load filename
    _ <- mapM_ (eval env) f
    return Void
eval env (List [Atom "load-print", String filename]) =
    load filename >>= fmap List . mapM (eval env)
eval env (List (Atom "vector-set!" : args)) = case args of
    [Atom var, a, b] -> do
        bound     <- liftIO $ isBound env var
        Integer n <- eval env a
        val       <- eval env b
        bndgs     <- liftIO $ readIORef (bindings env)
        let bndg = Data.Map.lookup var bndgs
        case bndg of
            Just v -> do
                vec@(Vector _) <- liftIO $ readIORef v
                updateVector env var vec n val
                return Void
            Nothing ->
                throwError $ TypeMismatch "vector, integer, integer" $ List
                    [val, a, b]
    [a, b, c] ->
        throwError $ TypeMismatch "vector, integer, integer" $ List [a, b, c]
    a -> throwError $ NumArgs (MinMax 3 3) (length args) a
  where
    updateVector
        :: Env
        -> String
        -> LispVal
        -> Integer
        -> LispVal
        -> IOThrowsError LispVal
    updateVector env' var' vec n v =
        let (Vector arr) = vec
        in  if n < fromIntegral (length arr) && n > 0
                then eval env' (Vector $ arr // [(n, v)]) >>= setVar env' var'
                else throwError $ outOfBoundsError "vector-ref" n arr
eval env (List (function : args)) = do
    func    <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

-- getRef :: Env -> String -> IOThrowsError LispVal
-- getRef env var = do
--   refs <- liftIO $ readIORef (references env)
--   case (Data.Map.lookup var refs) of
--     Just ptr -> liftIO $ readIORef ptr
--     _ ->
--       case parent env of
--         Nothing -> throwError $ UnboundVar "Getting an unbound variable" var
--         Just parent' -> getRef parent' var
evalList :: Env -> IOPrimitiveFunc
evalList _   []       = return Void
evalList env [a     ] = eval env a
evalList env (y : ys) = eval env y >> evalList env ys

getHeads :: IOPrimitiveFunc
getHeads []                  = return $ List []
getHeads (List (x : _) : ys) = do
    List result <- getHeads ys
    return $ List (x : result)
getHeads _ = throwError $ Default "Unexpected error in let"

getTails :: IOPrimitiveFunc
getTails []                  = return $ List []
getTails (List [_, xs] : ys) = do
    List result <- getTails ys
    return $ List (xs : result)
getTails _ = throwError $ Default "Unexpected error in let"

ensureAtom :: LispVal -> IOThrowsError LispVal
ensureAtom n@(Atom _) = return n
ensureAtom _          = throwError $ Default "Type error"

extractVar :: LispVal -> String
extractVar (Atom atom) = atom

makeFunc
    :: Monad m
    => String
    -> Maybe String
    -> Env
    -> [LispVal]
    -> [LispVal]
    -> m LispVal
makeFunc name' varargs env params' body' =
    return $ Func name' (map showVal params') varargs body' env

makeNormalFunc :: String -> Env -> [LispVal] -> IOPrimitiveFunc
makeNormalFunc name' = makeFunc name' Nothing

makeVarArgs :: String -> LispVal -> Env -> [LispVal] -> IOPrimitiveFunc
makeVarArgs name' = makeFunc name' . Just . showVal

isBoundLocal :: Env -> String -> IO Bool
isBoundLocal env var = do
    bindings' <- readIORef (bindings env)
    return $ case Data.Map.lookup var bindings' of
        Nothing -> False
        Just _  -> True

isBound :: Env -> String -> IO Bool
isBound env var = do
    localBound <- isBoundLocal env var
    if localBound
        then return True
        else case parent env of
            Nothing      -> return False
            Just parent' -> isBound parent' var

getVar :: Env -> String -> IOThrowsError LispVal
getVar env var = do
    bindings' <- liftIO $ readIORef (bindings env)
    refs      <- liftIO $ readIORef (references env)
    case Data.Map.lookup var bindings' of
        Just a  -> liftIO $ readIORef a
        Nothing -> case parent env of
            Nothing ->
                throwError $ UnboundVar "Getting an unbound variable" var
            Just parent' -> getVar parent' var

-- setReference :: Env -> String -> LispVal -> IOThrowsError LispVal
-- setReference env var value = do
--   refs <- liftIO $ readIORef (references env)
--   bndgs <- liftIO $ readIORef (bindings env)
--   let ref = Data.Map.lookup var refs
--   let bndg = Data.Map.lookup var bndgs
--   case (ref, bndg) of
--     (Just ptr, _) -> do
--       Pointer {pointerVar = pv, pointerEnv = pe} <- liftIO $ readIORef ptr
--       setVar pe pv value
--     (_, Just a) -> do
--       liftIO $ writeIORef a value
--       return Void
--     (_, _) ->
--       case parent env of
--         Nothing -> throwError $ UnboundVar "Getting an unbound variable" var
--         Just parent' -> setVar parent' var value
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar env var value = do
    bindings' <- liftIO $ readIORef (bindings env)
    case Data.Map.lookup var bindings' of
        Just a -> do
            liftIO $ writeIORef a value
            return Void
        Nothing -> case parent env of
            Nothing ->
                throwError $ UnboundVar "Getting an unbound variable" var
            Just parent' -> setVar parent' var value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar env var value = do
    bindings'             <- liftIO $ readIORef (bindings env)
    alreadyLocallyDefined <- liftIO $ isBoundLocal env var
    if alreadyLocallyDefined
        then
            throwError
            $  Default
            $  "Duplicate definition for identifier in: "
            ++ var
        else liftIO $ do
            valueRef <- newIORef value
            liftIO $ writeIORef (bindings env) (insert var valueRef bindings')
            return Void

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bindings' = do
    bindingsList <- mapM addBinding bindings'
    bindingsMap  <- newIORef $ fromList bindingsList
    refsList     <- mapM addBinding $ filter (isObject . snd) bindings'
    refsMap      <- newIORef $ fromList refsList
    return Frame
        { parent     = Just env
        , bindings   = bindingsMap
        , references = refsMap
        }
  where
    addBinding :: (String, LispVal) -> IO (String, IORef LispVal)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)
    isPointer :: LispVal -> Bool
    isPointer (Pointer _ _) = True
    isPointer _             = False

isObject :: LispVal -> Bool
isObject (List _        ) = True
isObject (DottedList _ _) = True
isObject (String _      ) = True
isObject (Vector _      ) = True
isObject (Pointer _ _   ) = True
isObject _                = False

--------------
-- Apply
--------------
derefPtrs :: [LispVal] -> LispVal -> IOThrowsError LispVal
derefPtrs ps (List l) = do
    result <- mapM (derefPtrs ps) l
    return $ List result
derefPtrs ps ptr@(Pointer p env) = if containsPtr ps ptr
    then return ptr -- Avoid cycle
    else do
        result <- getVar env p
        derefPtrs (ptr : ps) result
derefPtrs ps (DottedList ls l) = do
    ds <- mapM (derefPtrs ps) ls
    d  <- derefPtrs ps l
    return $ DottedList ds d
derefPtrs ps (Vector v) = do
    let vs = elems v
    ds <- mapM (derefPtrs ps) vs
    return $ Vector $ listArray (0, fromIntegral $ length vs - 1) ds
derefPtrs _ v = return v

containsPtr :: [LispVal] -> LispVal -> Bool
containsPtr (Pointer pa ea : ps) p@(Pointer pb eb) = do
    let found = (pa == pb) && bindings ea == bindings eb
    found || containsPtr ps p
containsPtr _ _ = False

apply :: LispVal -> IOPrimitiveFunc
apply (PrimitiveFunc _ func) args = do
    List derefedArgs <- derefPtrs [] (List args)
    liftThrows $ func derefedArgs
apply (Func _ params' varargs body' closure') args = do
    List derefedArgs <- derefPtrs [] (List args)
    if num params' /= num derefedArgs && isNothing varargs
        then throwError $ NumArgs (Min $ length params')
                                  (length derefedArgs)
                                  derefedArgs
        else
            liftIO (bindVars closure' $ zip params' args)
            >>= bindVarArgs varargs
            >>= evalBody
  where
    remainingArgs = drop (length params') args
    num           = toInteger . length
    evalBody env = last <$> mapM (eval env) body'
    bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing      -> return env
apply (IOFunc _ func) args = do
    List derefedArgs <- derefPtrs [] (List args)
    func derefedArgs
apply f _ =
    throwError
        $  Default
        $  "application: not a procedure; "
        ++ "expected a procedure that can be applied to arguments; given: "
        ++ show f

applyProc :: IOPrimitiveFunc
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc _                 = throwError $ Default "applyProc error"

readProc :: IOPrimitiveFunc
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc _           = throwError $ Default "readProc error"

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: IOPrimitiveFunc
readAll [String filename] = List <$> load filename
readAll _                 = throwError $ Default "readAll error"

--------------
-- Env
-- Bindings
-- Primitives
--------------
nullEnv :: IO Env
nullEnv = do
    bindings' <- newIORef empty
    refs      <- newIORef empty
    return Frame {parent = Nothing, bindings = bindings', references = refs}

primitiveBindings :: IO Env -- TODO: Move. Here for now just to prevent circular dependency
primitiveBindings = nullEnv >>= flip
    bindVars
    (  map
            (makeFunc' IOFunc)
            (  ioPrimitives
            ++ [ ("apply"   , applyProc)
               , ("read"    , readProc)
               , ("read-all", readAll)
               , ("read"    , readProc)
               , ("read-all", readAll)
               ]
            )
    ++ map (makeFunc' PrimitiveFunc) primitives
    )
  where
    makeFunc'
        :: (String -> ([LispVal] -> a) -> LispVal)
        -> (String, [LispVal] -> a)
        -> (String, LispVal)
    makeFunc' constructor (var, func) = (var, constructor var func)

loadStdLib :: Env -> IO Env
loadStdLib env = do
    filename <- liftIO $ getDataFileName "lib/stdlib.scm"
    _        <- evalString env $ "(load \"" ++ filename ++ "\")"
    return env

--------------
-- Run
-- Repl
--------------
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser input of
    Left  (err, _)   -> throwError $ DataTypes.Parser err
    Right [(val, _)] -> return val
    _                -> throwError $ Default "Read error"

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr (skipMany space))

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= evalLisp env

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings
        >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    _ <- loadStdLib env
    runIOThrows $ show' <$> evalLisp
        env
        (List [Atom "load-print", String (head args)])
    return ()
  where
    show' :: LispVal -> String
    show' (List contents) = unwordsList' contents
    show' _               = ""
    unwordsList' :: [LispVal] -> String
    unwordsList' = unlines . map showValNewline . filter printable
    showValNewline :: LispVal -> String
    showValNewline Void = ""
    showValNewline v    = showVal v
    printable :: LispVal -> Bool
    printable Void = False
    printable _    = True

runRepl :: InputT IO ()
runRepl = do
    env <- lift $ primitiveBindings >>= loadStdLib
    repl env
  where
    repl :: Env -> InputT IO ()
    repl env = do
        minput <- getInputLine "Lisp>>> "
        case minput of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                lift $ evalAndPrint env input
                repl env

