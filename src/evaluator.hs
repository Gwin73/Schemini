module Evaluator (evalExprList) where

import Control.Monad.Except (lift, liftIO, throwError)
import Control.Monad.Reader (ask, local)
import qualified Data.Map as M
import LispData
import Parser (parseExpr)

evalExprList :: [LispVal] -> Eval LispVal
evalExprList (List [Atom "load", Atom fileName] : rest) = do
    f <- liftIO $ readFile $ fileName ++ ".scm"
    lib <- either (throwError . ParseExcept) return (parseExpr f)
    case rest of 
        [] -> lift $ return $ List []
        _  -> evalExprList $ lib ++ rest
evalExprList (List [Atom "def", Atom var, expr] : rest) = do
    env <- ask
    if var `M.member` env
        then lift $ throwError $ UnboundVar "Defining bound variable" var
        else do 
            val <- local (const $ M.insert var Alloc env) (eval expr)
            let envFunc = (const $ M.insert var val env) in
                (case rest of
                    [] -> lift $ return $ val
                    x -> local envFunc $ evalExprList x)
evalExprList ((List ((Atom "begin") : rest1)) : rest2) = do 
    (evalExprList $ rest1 ++ rest2)
    
evalExprList all@(List (Atom name : rest1) : rest2 : rest3) = do
    env <- ask
    case M.lookup name env of 
        (Just macro@(Macro _ _ _ _)) -> (expandMacro macro rest1) >>= \ x-> evalExprList $ x : rest2 : rest3
        _ -> lift $ throwError $ BadSpecialForm $ List $ Atom "..." : all
evalExprList [x] = eval x
evalExprList badform = lift $ throwError $ BadSpecialForm $ List $ Atom "..." : badform

eval :: LispVal -> Eval LispVal
eval (Atom var) = ask >>= \env -> maybe 
    te 
    (\x-> if not $ isAlloc x then lift $ return x else te) 
    (M.lookup var env)
    where te = lift $ throwError $ UnboundVar "Getting unbound variable" var
eval val@(Bool _) = lift $ return val
eval val@(Int _) = lift $ return val
eval val@(String _) = lift $ return val
eval (List [Atom "quote", val]) = lift $ return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool True -> eval conseq
        Bool False -> eval alt 
        x -> lift $ throwError $ TypeMismatch "bool" x
eval (List [Atom "lambda", List params, body]) = ask >>= lift . return . (Lambda (map show params) [] body)
eval (List [Atom "lambda", Atom params, body]) = ask >>= lift . return . (Lambda [] params body)
eval (List [Atom "macro", List params, body]) = ask >>= lift . return . (Macro (map show params) [] body)
eval (List [Atom "macro", Atom params, body]) = ask >>= lift . return . (Macro [] params body)
eval (List [Atom "load", Atom fileName]) = lift $ return $ List []
eval (List [Atom "def", Atom var, expr]) = eval expr
eval (List [Atom "apply", func, arg]) = do
    evaledArg <- eval arg
    case evaledArg of 
        List args -> eval $ List $ func : args
        x -> lift $ throwError $ TypeMismatch "list" x
eval (List (name : args)) = do
    n <- eval name
    case n of
        (Macro _ _ _ _) -> applyMacro n args
        _ -> mapM eval args >>= applyFunction n
eval badform = lift $ throwError $ BadSpecialForm badform

applyFunction, applyMacro, expandMacro :: LispVal -> [LispVal] -> Eval LispVal
applyFunction (Function f) args = either throwError return (f args) 
applyFunction (IOFunction f) args = lift $ f args
applyFunction (Lambda params varParams body localEnv) args = 
    evalFM params varParams body localEnv args
applyFunction notF _ = lift $ throwError $ TypeMismatch "function" notF

applyMacro macro@(Macro params varParams body localEnv) args = do
    (expandMacro macro args) >>= eval 
applyMacro notM _ = lift $ throwError $ TypeMismatch "macro" notM

expandMacro (Macro params varParams body localEnv) args = 
    evalFM params varParams body localEnv args

evalFM :: [String] -> String -> LispVal -> Env -> [LispVal] -> Eval LispVal
evalFM params varParams body localEnv args = do
    env <- ask
    if length params /= length args && params /= []
        then lift $ throwError $ NumArgs (length params) args
        else let envFunc = (const $ (M.insert varParams (List args) (M.fromList $ zip params args)) `M.union` (update localEnv env)) 
             in local envFunc (eval body)

update localEnv env = (env `M.intersection` (M.filter isAlloc localEnv)) `M.union` localEnv
    
isAlloc Alloc = True
isAlloc _ = False