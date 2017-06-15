{-# LANGUAGE FlexibleContexts #-}
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Functor.Identity (Identity)
import Control.Monad (forM)
import Control.Monad.Except
import qualified Data.Map as M
import Control.Monad.Reader
 
main = forM (
    ["(cdr '(1 2))", "(print-line \"asd\")", "(print-line (read-line))"])
    (\x -> (runExceptT $ interp x) >>= (putStrLn . show))

interp :: String -> ExceptT LispExcept IO LispVal
interp input = either 
        (throwError . ParseExcept) 
        (\x -> runReaderT (evalExprList x) stdEnv) 
        (parseExpr input)

parseExpr :: String -> Either ParseError LispVal
parseExpr = parse (whiteSpace >> lexeme expr) "Schemini"

expr :: Parser LispVal
expr 
    =  atom      
    <|> bool
    <|> integer
    <|> string'
    <|> quoted
    <|> list

atom :: Parser LispVal
atom = identifier >>= (return . Atom)

bool :: Parser LispVal
bool = (reserved "#t" >> (return $ Bool True)) <|> (reserved "#f" >> (return $ Bool False))

integer :: Parser LispVal
integer = lexeme $ many1 digit >>= return . Integer . read

string' :: Parser LispVal
string' = lexeme $ between (char '\"') (char '\"') (many (try escapeChar <|> noneOf ['\"', '\\'])) >>= (return . String)
    where escapeChar = char '\\' >> oneOf ['\\', '"'] >>= return

quoted :: Parser LispVal
quoted = lexeme $ string "'" >> expr >>= \x -> return $ List [Atom "quote", x]

list :: Parser LispVal
list = parens $ many expr >>= return . List

Tok.TokenParser {Tok.parens = parens, Tok.identifier = identifier, Tok.reserved = reserved, Tok.lexeme = lexeme, Tok.whiteSpace = whiteSpace} = 
    Tok.makeTokenParser schemeDef  

schemeDef :: Tok.GenLanguageDef String () Identity
schemeDef = Lang.emptyDef 
    { Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~+-"
    , Tok.identLetter = digit <|> Tok.identStart schemeDef
    , Tok.reservedNames = ["#t", "#f"]
    }

evalExprList :: LispVal -> ReaderT Env (ExceptT LispExcept IO) LispVal
evalExprList (List ((List [Atom "define", Atom var, expr] : rest))) = do
    env <- ask
    val <- eval expr
    let envFunc = (const $ M.insert var val env) in
        (case rest of
            [] -> lift $ return $ List []
            [x] -> local envFunc (evalExprList x)
            x -> local envFunc (evalExprList $ List x))
evalExprList x = eval x

eval :: LispVal -> ReaderT Env (ExceptT LispExcept IO) LispVal
eval (Atom var) = do
    env <- ask
    case M.lookup var env of
        Nothing -> lift $ throwError $ UnboundVar "Getting unbound variable" var
        Just x -> lift $ return x 
eval val@(Bool _) = lift $ return val
eval val@(Integer _) = lift $ return val
eval val@(String _) = lift $ return val
eval (List [Atom "quote", val]) = lift $ return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool True -> eval conseq
        Bool False -> eval alt 
        x -> lift $ throwError $ TypeMismatch "bool" x
eval (List (Atom "lambda" : List params : body)) = do
    env <- ask
    lift $ return $ Procedure (map show params) body env
eval (List [Atom "define", Atom var, expr]) = eval expr
eval (List (proc : args)) = do
    p <- eval proc
    as <- mapM eval args
    applyProc p as
eval badform = lift $ throwError $ BadSpecialForm badform  

applyProc :: LispVal -> [LispVal] -> ReaderT Env (ExceptT LispExcept IO) LispVal
applyProc (PProcedure f) args = lift $ f args
applyProc (Procedure params body env) args = do
    if length params /= length args
        then lift $ throwError $ NumArgs (length params) args
        else local (const $ M.fromList (zip params args) `M.union` env) (liftM last $ mapM eval body)
applyProc notP _ = lift $ throwError $ TypeMismatch "procedure" notP


stdEnv = M.fromList 
    [{-("+", PProcedure $ intFoldop (+)), 
    ("-", PProcedure $ intFoldop (-)), 
    ("*", PProcedure $ intFoldop (*)), 
    ("/", PProcedure $ intFoldop div),
    ("mod", PProcedure $ intFoldop mod)--,
    ("=", PProcedure $ intCompop (==)),
    (">", PProcedure $ intCompop (>)),
    (">=", PProcedure $ intCompop (>=)),
    ("<", PProcedure $ intCompop (<)),
    ("<=", PProcedure $ intCompop (<=)),
    ("integer?", PProcedure $ lispvalQ unpackInteger),
    ("and", PProcedure $ boolFoldop (&&)),
    ("or", PProcedure $ boolFoldop (||)),
    ("boolean?", PProcedure $ lispvalQ unpackBool),
    ("string-append", PProcedure $ strFoldop (++)),
    ("integer->string", PProcedure $ unop String unpackInteger show),
    ("string-length", PProcedure $ unop Integer unpackStr (toInteger . length)),
    ("string=?", PProcedure $ stringCompOp (==)),
    ("string?", PProcedure $ lispvalQ unpackStr),
    ("car", PProcedure car),-}
    ("cdr", PProcedure cdr),
    ("print-line", PProcedure printLine),
     ("read-line", PProcedure readLine)
   -- ("cons", PProcedure cons),
   -- ("list?", PProcedure $ lispvalQ unpackList),
   -- ("equal?", PProcedure equal)
    ]

intFoldop = foldop Integer unpackInteger
boolFoldop = foldop Bool unpackBool
strFoldop = foldop String unpackStr

foldop :: (a -> LispVal) -> (LispVal -> Either LispExcept a) -> (a -> a -> a) -> [LispVal] -> Either LispExcept LispVal
foldop _ _ _ [] = throwError $ NumArgs 2 []
foldop _ _ _ s@[_] = throwError $ NumArgs 2 s
foldop packer unpacker op params = mapM unpacker params >>= return . packer . foldl1 op

intCompop = compop unpackInteger
stringCompOp = compop unpackBool 

compop :: (LispVal -> Either LispExcept a) -> (a -> a -> Bool) -> [LispVal] -> Either LispExcept LispVal
compop _ _ [] = throwError $ NumArgs 2 []
compop _ _ s@[_] = throwError $ NumArgs 2 s
compop unpacker op params = mapM unpacker params >>= \x -> return $ Bool $ and (zipWith (op) (x) (tail x))

unop :: (b -> LispVal) -> (LispVal -> Either LispExcept a) -> (a -> b) -> [LispVal] -> Either LispExcept LispVal
unop packer unpacker op [arg] = unpacker arg >>= return . packer . op
unop _ _ _ badArgs = throwError $ NumArgs 1 badArgs

unpackInteger :: LispVal -> Either LispExcept Integer
unpackInteger (Integer n) = return n
unpackInteger notInt = throwError $ TypeMismatch "integer" notInt

unpackBool :: LispVal -> Either LispExcept Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackStr :: LispVal -> Either LispExcept String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackList :: LispVal -> Either LispExcept [LispVal]
unpackList (List l) = return l
unpackList notlst = throwError $ TypeMismatch "list" notlst

car :: [LispVal] -> Either LispExcept LispVal
car [(List (x : xs))] = return x
car [badArg] = throwError $ TypeMismatch "list" badArg
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ExceptT LispExcept IO LispVal
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
equal [Integer i1, Integer i2] = return $ Bool $ i1 == i2
equal [String s1, String s2] = return $ Bool $ s1 == s2
equal [_, _] = return $ Bool False
equal badArgs =  throwError $ NumArgs 2 badArgs

printLine :: [LispVal] -> ExceptT LispExcept IO LispVal
printLine [String s] = liftIO $ putStrLn s >> (return $ Bool True) 
printLine [badArg] = throwError $ TypeMismatch "string" badArg
printLine badArgs = throwError $ NumArgs 1 badArgs

readLine :: [LispVal] -> ExceptT LispExcept IO LispVal
readLine [] = liftIO $ getLine >>= return . String
readLine badArgs = throwError $ NumArgs 0 badArgs

type Env = M.Map String LispVal

data LispVal 
    = Atom String
    | Bool Bool
    | Integer Integer
    | String String
    | List [LispVal]
    | PProcedure ([LispVal] -> ExceptT LispExcept IO LispVal)
    | Procedure [String] [LispVal] Env

instance Show LispVal where
    show v = case v of
        Atom s -> s
        Bool True -> "#t"
        Bool False -> "#f"
        Integer n -> show n
        String s -> "\"" ++ s ++ "\""
        List l -> "(" ++ (unwords . map show) l ++ ")"
        PProcedure _ -> "#<Standard procedure>"  
        Procedure _ _ _ -> "#<Lambda>"

data LispExcept
    = TypeMismatch String LispVal
    | NumArgs Int [LispVal]
    | UnboundVar String String
    | BadSpecialForm LispVal
    | ParseExcept ParseError

instance Show LispExcept where 
    show e = case e of
        TypeMismatch expected found -> "Invalid type: expected " ++ expected ++ ", found " ++ show found
        NumArgs expected found -> "Wrong number of arguments: expected " ++ show expected ++ ", found " ++ show found
        UnboundVar message varname -> message ++ ": " ++ varname
        BadSpecialForm form -> "Unrecognized special form: " ++ show form
        ParseExcept err -> "ParseError: " ++ show err