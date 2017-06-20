{-# LANGUAGE FlexibleContexts #-}
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Functor.Identity (Identity)
import Control.Monad (forM)
import Control.Monad.Except
import qualified Data.Map as M
import Control.Monad.Reader
import System.Environment
import System.Directory
 
main :: IO ()
main = do
    args <- getArgs
    if length args == 1
        then do
            exists <- doesFileExist $ args !! 0
            if exists
                then readFile (args !! 0) >>= runExceptT . interp >>= putStrLn . (either show show)
                else putStrLn "File does not exist"
        else putStrLn "Specify path to scm file."

interp :: String -> ExceptT LispExcept IO LispVal
interp input = either 
        (throwError . ParseExcept) 
        (\x -> (runReaderT (eval x) stdEnv))
        (parseExpr input)

parseExpr :: String -> Either ParseError LispVal
parseExpr = parse (whiteSpace >> lexeme expr) "Schemini"

expr :: Parser LispVal
expr 
    =  atom      
    <|> bool
    <|> integer
    <|> str
    <|> lambda
    <|> quoted
    <|> list

atom :: Parser LispVal
atom = identifier >>= (return . Atom)

bool :: Parser LispVal
bool = (reserved "#t" >> (return $ Bool True)) <|> (reserved "#f" >> (return $ Bool False))

integer :: Parser LispVal
integer = lexeme $ many1 digit >>= return . Int . read

str :: Parser LispVal
str = lexeme $ between (char '\"') (char '\"') (many (try escapeChar <|> noneOf ['\"', '\\'])) >>= (return . String)
    where escapeChar = char '\\' >> oneOf ['\\', '"'] >>= return

quoted :: Parser LispVal
quoted = lexeme $ string "'" >> expr >>= \x -> return $ List [Atom "quote", x]

lambda :: Parser LispVal
lambda = lexeme $ string "\\" >> (return . Atom) "lambda"

list :: Parser LispVal
list = parens $ many expr >>= return . List

Tok.TokenParser {Tok.parens = parens, Tok.identifier = identifier, Tok.reserved = reserved, Tok.lexeme = lexeme, Tok.whiteSpace = whiteSpace} = 
    Tok.makeTokenParser schemeDef  

schemeDef :: Tok.GenLanguageDef String () Identity
schemeDef = Lang.emptyDef 
    { Tok.commentLine = ";"
    , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~+-|"
    , Tok.identLetter = digit <|> Tok.identStart schemeDef
    , Tok.reservedNames = ["#t", "#f"]
    }


eval :: LispVal -> ReaderT Env (ExceptT LispExcept IO) LispVal
eval (Atom var) = do
    env <- ask
    case M.lookup var env of
        Nothing -> lift $ throwError $ UnboundVar "Getting unbound variable" var
        Just x -> lift $ return x 
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
eval (List [Atom "lambda", List params, body]) = do
    env <- ask
    lift $ return $ Function (map show params) body env
eval (List [Atom "lambda", Atom params, body]) = do
    env <- ask
    lift $ return $ VariadicFunction params body env
eval (List [Atom "def", Atom var, expr]) = do
    env <- ask
    if var `M.member` env
        then lift $ throwError $ UnboundVar "Defining bound variable" var
        else eval expr
eval (List [Atom "load", Atom fileName]) = lift $ return $ List []
eval (List (Atom "begin" : expressions)) = evalExprList expressions
eval (List [Atom "apply", func, List args]) = eval (List (func : args))
eval (List (func : args)) = do
    p <- eval func
    as <- mapM eval args
    applyProc p as
eval badform = lift $ throwError $ BadSpecialForm badform

evalExprList :: [LispVal] -> ReaderT Env (ExceptT LispExcept IO) LispVal
evalExprList (List [Atom "def", Atom var, expr] : rest) = do
    env <- ask
    if var `M.member` env
        then lift $ throwError $ UnboundVar "Defining bound variable" var
        else do 
            val <- local (const $ M.insert var (List []) env) (eval expr) -- Define the lambda as [] in its own local enviroment, replaced in eval and needed for recursion
            let envFunc = (const $ M.insert var val env) in
                (case rest of
                    [] -> lift $ return $ val
                    [x] -> local envFunc (evalExprList [x])
                    x -> local envFunc (evalExprList x))
evalExprList (List [Atom "load", Atom fileName] : rest) = do
    f <- liftIO $ readFile $ fileName ++ ".scm" --TODO: Handle case where file doesnt exist
    lib <- either (throwError . ParseExcept) return (parseExpr f)
    case rest of 
        [] -> lift $ return $ List []
        _  -> case lib of
                (List a@(Atom "begin" : expressions)) -> evalExprList (expressions ++ rest)
                expr@(List _) -> evalExprList (expr : rest)
evalExprList [x] = eval x
evalExprList badform = lift $ throwError $ BadSpecialForm $ List (Atom "begin ..." : badform)

applyProc :: LispVal -> [LispVal] -> ReaderT Env (ExceptT LispExcept IO) LispVal
applyProc (StdFunction f) args = either (throwError) (return) (f args) 
applyProc (StdIOFunction f) args = lift $ f args
applyProc (Function params body localEnv) args = do
    env <- ask
    if length params /= length args
        then lift $ throwError $ NumArgs (length params) args
        else local (const $ M.fromList (zip params args) `M.union` ((env `M.intersection` localEnv)) `M.union` localEnv) (eval body)

applyProc (VariadicFunction params body localEnv) args = do
    env <- ask
    local (const $ (M.fromList [(params, List args)]) `M.union` ((env `M.intersection` localEnv)) `M.union` localEnv) (eval body)


applyProc notP _ = lift $ throwError $ TypeMismatch "function" notP

stdEnv = M.fromList 
    [("+", StdFunction $ intIntBinop (+)), 
    ("-", StdFunction $ intIntBinop (-)), 
    ("*", StdFunction $ intIntBinop (*)), 
    ("/", StdFunction $ intIntBinop div),
    ("mod", StdFunction $ intIntBinop mod),
    ("=", StdFunction $ intBoolBinop (==)),
    (">", StdFunction $ intBoolBinop (>)),
    (">=", StdFunction $ intBoolBinop (>=)),
    ("<", StdFunction $ intBoolBinop (<)),
    ("<=", StdFunction $ intBoolBinop (<=)),
    ("int?", StdFunction $ lispvalQ unpackInteger), --
    ("&&", StdFunction $ boolBoolBinop (&&)),
    ("||", StdFunction $ boolBoolBinop (||)),
    ("bool?", StdFunction $ lispvalQ unpackBool), --
    ("str-append", StdFunction $ binop String unpackStr (++)),
    ("int->str", StdFunction $ unop String unpackInteger show),
    ("str->int", StdFunction $ unop Int unpackStr read),
    ("str-length", StdFunction $ unop Int unpackStr (toInteger . length)),
    ("str=?", StdFunction $ binop Bool unpackStr (==)),
    ("str?", StdFunction $ lispvalQ unpackStr), --
    ("car", StdFunction car), --
    ("cdr", StdFunction cdr), --
    ("cons", StdFunction cons), --
    ("list?", StdFunction $ lispvalQ unpackList), --
    ("equal?", StdFunction equal),
    ("print-line", StdIOFunction printLine),
    ("read-line", StdIOFunction readLine)
    ]

unop :: (b -> LispVal) -> (LispVal -> Either LispExcept a) -> (a -> b) -> [LispVal] -> Either LispExcept LispVal
unop packer unpacker op [arg] = unpacker arg >>= return . packer . op
unop _ _ _ badArgs = throwError $ NumArgs 1 badArgs

intIntBinop = binop Int unpackInteger
boolBoolBinop = binop Bool unpackBool
intBoolBinop = binop Bool unpackInteger

binop :: (b -> LispVal) -> (LispVal -> Either LispExcept a) -> (a -> a -> b) -> [LispVal] -> Either LispExcept LispVal
binop packer unpacker op args@[arg1, arg2] = do
    uArg1 <- unpacker arg1
    uArg2 <- unpacker arg2
    return $ packer $ (uArg1 `op` uArg2)
binop _ _ _ badArgs = throwError $ NumArgs 2 badArgs

unpackInteger :: LispVal -> Either LispExcept Integer
unpackInteger (Int n) = return n
unpackInteger notInt = throwError $ TypeMismatch "int" notInt

unpackBool :: LispVal -> Either LispExcept Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackStr :: LispVal -> Either LispExcept String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "str" notStr

unpackList :: LispVal -> Either LispExcept [LispVal]
unpackList (List l) = return l
unpackList notlst = throwError $ TypeMismatch "list" notlst

car :: [LispVal] -> Either LispExcept LispVal
car [(List (x : xs))] = return x
car [badArg] = throwError $ TypeMismatch "list" badArg
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> Either LispExcept LispVal
cdr [(List (x : xs))] = return $ List xs
cdr [badArg] = throwError $ TypeMismatch "list" badArg
cdr badArgs = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> Either LispExcept LispVal
cons [x, List xs] = return $ List $ x : xs
cons [_, arg2] = throwError $ TypeMismatch "list" arg2
cons badArgs = throwError $ NumArgs 2 badArgs

lispvalQ :: (LispVal -> Either LispExcept a) -> [LispVal] -> Either LispExcept LispVal
lispvalQ unpacker [x] = either (\x->return $ Bool False) (\x->return $ Bool True) (unpacker x)
lispvalQ _ args = throwError $ NumArgs 2 args

equal :: [LispVal] -> Either LispExcept LispVal
equal [Atom a1, Atom a2] = return $ Bool $ a1 == a2
equal [Bool b1, Bool b2] = return $ Bool $ b1 == b2
equal [Int i1, Int i2] = return $ Bool $ i1 == i2
equal [String s1, String s2] = return $ Bool $ s1 == s2
equal [List l1, List l2] = return $ Bool $ (length l1 == length l2) && all (\(Right (Bool x)) -> x) (zipWith (\x y -> equal [x, y]) l1 l2)
equal [_, _] = return $ Bool False
equal badArgs = throwError $ NumArgs 2 badArgs

printLine :: [LispVal] -> ExceptT LispExcept IO LispVal
printLine [String s] = liftIO $ putStrLn s >> (return $ Bool True) 
printLine [badArg] = throwError $ TypeMismatch "str" badArg
printLine badArgs = throwError $ NumArgs 1 badArgs

readLine :: [LispVal] -> ExceptT LispExcept IO LispVal
readLine [] = liftIO $ getLine >>= return . String
readLine badArgs = throwError $ NumArgs 0 badArgs

type Env = M.Map String LispVal

data LispVal 
    = Atom String
    | Bool Bool
    | Int Integer
    | String String
    | List [LispVal]
    | StdFunction ([LispVal] -> Either LispExcept LispVal)
    | StdIOFunction ([LispVal] -> ExceptT LispExcept IO LispVal)
    | Function [String] LispVal Env
    | VariadicFunction String LispVal Env
    | Lambda

instance Show LispVal where
    show v = case v of
        Atom s -> s
        Bool True -> "#t"
        Bool False -> "#f"
        Int n -> show n
        String s -> "\"" ++ s ++ "\""
        List l -> "(" ++ (unwords . map show) l ++ ")"
        StdFunction _ -> "#<Standard function>"  
        StdIOFunction _ -> "#<Standard function>" 
        Function _ _ _ -> "#<Function>"
        VariadicFunction _ _ _ -> "#<Function>"

data LispExcept
    = TypeMismatch String LispVal
    | NumArgs Int [LispVal]
    | UnboundVar String String
    | BadSpecialForm LispVal
    | ParseExcept ParseError

instance Show LispExcept where 
    show e = case e of
        TypeMismatch expected found -> "EvalError: Invalid type: expected " ++ expected ++ ", found " ++ show found
        NumArgs expected found -> "EvalError: Wrong number of arguments: expected " ++ show expected ++ ", found " ++ show found
        UnboundVar message varname -> "EvalError: " ++ message ++ ": " ++ varname
        BadSpecialForm form -> "EvalError: Unrecognized special form: " ++ show form
        ParseExcept err -> "ParseError: " ++ show err