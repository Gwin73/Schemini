{-# LANGUAGE FlexibleContexts #-}
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Functor.Identity (Identity)
import Control.Monad.Except
import qualified Data.Map as M
import Control.Monad.Reader
import System.Environment
import System.Directory
import qualified Control.Exception as Exc

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "Specify path to scm file." 
        else do
            exists <- doesFileExist $ args !! 0
            if not exists
                then putStrLn "File does not exist"
                else do
                    s <- readFile (args !! 0)
                    (((runExceptT . interp) s) >>= (putStrLn . (either show show))) `Exc.catch` (\(Exc.SomeException e) -> putStrLn $ "RuntimeError: " ++ (show e))

interp :: String -> ExceptT LispExcept IO LispVal
interp input = either 
        (throwError . ParseExcept) 
        (\x -> (runReaderT (eval x) primEnv))
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
atom = identifier >>= return . Atom

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
eval (Atom var) = ask >>= \env -> maybe 
    (lift $ throwError $ UnboundVar "Getting unbound variable" var) 
    (lift . return) 
    (M.lookup var env)
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
eval (List [Atom "lambda", List params, body]) = ask >>= \env -> lift $ return $ Lambda (map show params) body env
eval (List [Atom "lambda", Atom params, body]) = ask >>= \env -> lift $ return $ VariadricLambda params body env
eval (List [Atom "def", Atom var, expr]) = do
    env <- ask
    if var `M.member` env
        then lift $ throwError $ UnboundVar "Defining bound variable" var
        else eval expr
eval (List [Atom "load", Atom fileName]) = lift $ return $ List []
eval (List (Atom "begin" : expressions)) = evalExprList expressions
eval (List [Atom "apply", func, arg]) = do
    evaledArg <- eval arg
    case evaledArg of 
        List args -> eval $ List $ func : args
        x -> lift $ throwError $ TypeMismatch "list" x
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
            val <- local (const $ M.insert var Alloc env) (eval expr)
            let envFunc = (const $ M.insert var val env) in
                (case rest of
                    [] -> lift $ return $ val
                    x -> local envFunc (evalExprList x))
evalExprList (List [Atom "load", Atom fileName] : rest) = do
    f <- liftIO $ readFile $ fileName ++ ".scm" 
    lib <- either (throwError . ParseExcept) return (parseExpr f)
    case rest of 
        [] -> lift $ return $ List []
        _  -> case lib of
                (List a@(Atom "begin" : expressions)) -> evalExprList (expressions ++ rest)
                expr@(List _) -> evalExprList (expr : rest)
evalExprList [x] = eval x
evalExprList badform = lift $ throwError $ BadSpecialForm $ List (Atom "begin ..." : badform)

applyProc :: LispVal -> [LispVal] -> ReaderT Env (ExceptT LispExcept IO) LispVal
applyProc (Function f) args = either (throwError) (return) (f args) 
applyProc (IOFunction f) args = lift $ f args
applyProc (Lambda params body localEnv) args = do
    env <- ask
    if length params /= length args
        then lift $ throwError $ NumArgs (length params) args
        else local (const $ M.fromList (zip params args) `M.union` (update localEnv env)) (eval body)
applyProc (VariadricLambda params body localEnv) args = do
    env <- ask
    local (const $ (M.fromList [(params, List args)]) `M.union` (update localEnv env)) (eval body)
applyProc notP _ = lift $ throwError $ TypeMismatch "function" notP

update :: Env -> Env -> Env
update localEnv env = (env `M.intersection` (M.filter isAlloc localEnv)) `M.union` localEnv
    where 
        isAlloc x = case x of 
            Alloc -> True
            _ -> False

primEnv = M.fromList 
    [("+", Function $ intIntBinop (+)), 
    ("-", Function $ intIntBinop (-)), 
    ("*", Function $ intIntBinop (*)), 
    ("/", Function $ intIntBinop div),
    ("mod", Function $ intIntBinop mod),
    ("=", Function $ intBoolBinop (==)),
    (">", Function $ intBoolBinop (>)),
    (">=", Function $ intBoolBinop (>=)),
    ("<", Function $ intBoolBinop (<)),
    ("<=", Function $ intBoolBinop (<=)),
    ("int?", Function $ lispvalQ unpackInt), 
    ("&&", Function $ boolBoolBinop (&&)),
    ("||", Function $ boolBoolBinop (||)),
    ("bool?", Function $ lispvalQ unpackBool), 
    ("str-append", Function $ binop unpackStr unpackStr String (++)),
    ("int->str", Function $ unop unpackInt String show),
    ("str->int", Function $ unop unpackStr Int read),
    ("str-length", Function $ unop unpackStr Int (toInteger . length)),
    ("str=?", Function $ binop unpackStr unpackStr Bool (==)),
    ("str?", Function $ lispvalQ unpackStr), 
    ("car", Function $ unop unpackLst id (head)),
    ("cdr", Function $ unop unpackLst List (tail)),
    ("cons", Function $ binop Right unpackLst List (:)),
    ("list?", Function $ lispvalQ unpackLst), 
    ("equal?", Function equal),
    ("print-line", IOFunction printLine),
    ("read-line", IOFunction readLine)
    ]

unop :: Unpacker a -> Packer b -> (a -> b) -> [LispVal] -> Either LispExcept LispVal
unop unpacker packer op [arg] = unpacker arg >>= return . packer . op
unop _ _ _ badArgs = throwError $ NumArgs 1 badArgs

intIntBinop = binop unpackInt unpackInt Int
boolBoolBinop = binop unpackBool unpackBool Bool
intBoolBinop = binop unpackInt unpackInt Bool

binop :: Unpacker a -> Unpacker b -> Packer c -> (a -> b -> c) -> [LispVal] -> Either LispExcept LispVal
binop unpacker1 unpacker2 packer op args@[arg1, arg2] = do
    uArg1 <- unpacker1 arg1
    uArg2 <- unpacker2 arg2
    return $ packer $ (uArg1 `op` uArg2)
binop _ _ _ _ badArgs = throwError $ NumArgs 2 badArgs

unpackInt :: Unpacker Integer
unpackInt (Int n) = return n
unpackInt notInt = throwError $ TypeMismatch "int" notInt

unpackBool :: Unpacker Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackStr :: Unpacker String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "str" notStr

unpackLst :: Unpacker [LispVal]
unpackLst (List l) = return l
unpackLst notlst = throwError $ TypeMismatch "list" notlst

lispvalQ :: Unpacker a -> [LispVal] -> Either LispExcept LispVal
lispvalQ unpacker [x] = either (const $ return $ Bool False) (const $ return $ Bool True) (unpacker x)
lispvalQ _ args = throwError $ NumArgs 2 args

equal :: [LispVal] -> Either LispExcept LispVal
equal [Atom a1, Atom a2] = return $ Bool $ a1 == a2
equal [Bool b1, Bool b2] = return $ Bool $ b1 == b2
equal [Int i1, Int i2] = return $ Bool $ i1 == i2
equal [String s1, String s2] = return $ Bool $ s1 == s2
equal [List l1, List l2] = return $ Bool $ (length l1 == length l2) && all (\(Right (Bool x)) -> x) (zipWith (\x y -> equal [x, y]) l1 l2)
equal [_, _] = return $ Bool False
equal badArgs = throwError $ NumArgs 2 badArgs

printLine, readLine :: [LispVal] -> ExceptT LispExcept IO LispVal
printLine [String s] = liftIO $ putStrLn s >> (return $ List []) 
printLine [badArg] = throwError $ TypeMismatch "str" badArg
printLine badArgs = throwError $ NumArgs 1 badArgs

readLine [] = liftIO $ getLine >>= return . String
readLine badArgs = throwError $ NumArgs 0 badArgs

type Env = M.Map String LispVal
type Unpacker a = (LispVal -> Either LispExcept a)
type Packer a = (a -> LispVal)

data LispVal 
    = Atom String
    | Bool Bool
    | Int Integer
    | String String
    | List [LispVal]
    | Function ([LispVal] -> Either LispExcept LispVal)
    | IOFunction ([LispVal] -> ExceptT LispExcept IO LispVal)
    | Lambda [String] LispVal Env
    | VariadricLambda String LispVal Env
    | Alloc

instance Show LispVal where
    show v = case v of
        Atom s -> s
        Bool True -> "#t"
        Bool False -> "#f"
        Int n -> show n
        String s -> "\"" ++ s ++ "\""
        List l -> "(" ++ (unwords . map show) l ++ ")"
        Function _ -> "#<Function>"  
        IOFunction _ -> "#<Function>" 
        Lambda _ _ _ -> "#<Lambda>"
        VariadricLambda _ _ _ -> "#<Lambda>"

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