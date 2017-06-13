{-# LANGUAGE FlexibleContexts #-}
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Functor.Identity (Identity)
import Control.Monad (forM)
import Control.Monad.Except
import qualified Data.Map as M
import Control.Monad.Reader
 
main = forM (["if", "'if", "#t", "'#t", "123", "'123", "\"asd\"", "'\"asd\"", "()", "'()", "'(asd (2 \"asd\"))"]
    ++ ["(if #t 1 2)", "(if #f 1 2)"] ++ ["pi"] ++ ["(+ 1 2)", "(- 1 2)", "(* 1 2)", "(/ 1 2)", "(mod 3 2)", "(+ 1 2 3)", "(* 2)", "(*)", "(1 2 3)"]
    ++ ["(and #t #t)", "(and #t #f)", "(and #f #f)", "(or #t #t)", "(or #t #f)", "(or #f #f)", "(and #t #t #t)", "(and #t)", "(and)"]
    ++ ["(= 1 3)", "(= 1 1)", "(> 1 3)", "(>= 1 3)", "(< 1 3)", "(<= 1 3)", "(= 1 1 1)", "(=)", "(= 1)"]) 
    (print . interp)

interp :: String -> Either LispExcept LispVal
interp input = either 
        (throwError . ParseExcept) 
        (\x -> runReaderT (eval x) emptyEnv) 
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

Tok.TokenParser {Tok.parens = parens, Tok.identifier = identifier, Tok.reserved = reserved, Tok.lexeme = lexeme, Tok.reservedOp = reservedOp, Tok.whiteSpace = whiteSpace} = 
    Tok.makeTokenParser schemeDef  

schemeDef :: Tok.GenLanguageDef String () Identity
schemeDef = Lang.emptyDef 
    { Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~+-" --Not entirely correct in r5rs an identifier is Tok.identifier or + or - or ...
    , Tok.identLetter = digit <|> Tok.identStart schemeDef
    , Tok.reservedNames = ["#t", "#f", "'"]
    }


eval :: LispVal -> ReaderT (M.Map String LispVal) (Either LispExcept) LispVal
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
eval (List (proc : args)) = do
    p <- eval proc
    as <- mapM eval args
    applyProc p as
eval badform = lift $ throwError $ BadSpecialForm badform      

applyProc (PProcedure f) params = lift $ f params
applyProc notP _ = lift $ throwError $ TypeMismatch "procedure" notP


intIntOp = op Integer unpackInteger
boolBoolOp = op Bool unpackBool

op :: (a -> LispVal) -> (LispVal -> Either LispExcept a) -> (a -> a -> a) -> [LispVal] -> Either LispExcept LispVal
op _ _ _ [] = throwError $ NumArgs 2 []
op _ _ _ s@[_] = throwError $ NumArgs 2 s
op packer unpacker op params = mapM unpacker params >>= return . packer . foldl1 op

intBoolOp :: (Integer -> Integer -> Bool) -> [LispVal] -> Either LispExcept LispVal
intBoolOp _ [] = throwError $ NumArgs 2 []
intBoolOp _ s@[_] = throwError $ NumArgs 2 s
intBoolOp op params = mapM unpackInteger params >>= \x -> return $ Bool $ and (zipWith (op) (x) (tail x))

unpackInteger :: LispVal -> Either LispExcept Integer
unpackInteger (Integer n) = return n
unpackInteger notInt = throwError $ TypeMismatch "integer" notInt

unpackBool :: LispVal -> Either LispExcept Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool


emptyEnv = M.fromList 
    [("pi", Integer 3), 
    ("+", PProcedure $ intIntOp (+)), 
    ("-", PProcedure $ intIntOp (-)), 
    ("*", PProcedure $ intIntOp (*)), 
    ("/", PProcedure $ intIntOp div),
    ("mod", PProcedure $ intIntOp mod),
    ("=", PProcedure $ intBoolOp (==)),
    (">", PProcedure $ intBoolOp (>)),
    (">=", PProcedure $ intBoolOp (>=)),
    ("<", PProcedure $ intBoolOp (<)),
    ("<=", PProcedure $ intBoolOp (<=)),
    ("and", PProcedure $ boolBoolOp (&&)),
    ("or", PProcedure $ boolBoolOp (||))
    ]

data LispVal 
    = Atom String
    | Bool Bool
    | Integer Integer
    | String String
    | List [LispVal]
    | PProcedure ([LispVal] -> Either LispExcept LispVal)

instance Show LispVal where
    show v = case v of
        Atom s -> "Atom: " ++ s
        Bool True -> "Bool: #t"
        Bool False -> "Bool: #f"
        Integer n -> "Integer: " ++ show n
        String s -> "String: " ++ "\"" ++ s ++ "\""
        List l -> "List: " ++ "(" ++ (unwords . map show) l ++ ")"
        PProcedure _ -> "<Standard procedure>"    

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