import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Functor.Identity (Identity)
import Control.Monad (forM)
import Control.Monad.Except
 
main = forM (["if", "'if", "#t", "'#t", "123", "'123", "\"asd\"", "'\"asd\"", "()", "'()", "'(asd (2 \"asd\"))"]
    ++ ["(if #t 1 2)", "(if #f 1 2)"] ++ ["\" \\\\asd\\\\ \""]) 
    (putStrLn . show . parseExpr)

readExpr :: String -> Either LispExcept LispVal
readExpr input = case parseExpr input of
    Left err -> throwError $ ParseExcept err
    Right val -> interp val

parseExpr :: String -> Either ParseError LispVal
parseExpr input = parse (whiteSpace >> lexeme expr) "Scheme" input

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


interp :: LispVal -> Either LispExcept LispVal
interp val@(Bool _) = return val
interp val@(Integer _) = return val
interp val@(String _) = return val
interp (List [Atom "quote", val]) = return val
interp (List [Atom "if", pred, conseq, alt]) = do
    result <- interp pred
    case result of
        Bool True -> interp conseq
        Bool False -> interp alt 
        x -> throwError $ TypeMismatch "bool" x
interp badform = throwError $ BadSpecialForm badform      


data LispVal 
    = Atom String
    | Bool Bool
    | Integer Integer
    | String String
    | List [LispVal]

instance Show LispVal where
    show v = case v of
        Atom s -> "Atom: " ++ s
        Bool True -> "Bool: #t"
        Bool False -> "Bool: #f"
        Integer n -> "Integer: " ++ show n
        String s -> "String: " ++ "\"" ++ s ++ "\""
        List l -> "List: " ++ "(" ++ (unwords . map show) l ++ ")"    

data LispExcept
    = TypeMismatch String LispVal
    | BadSpecialForm LispVal
    | ParseExcept ParseError
    | OtherExcept String

instance Show LispExcept where 
    show e = case e of
        TypeMismatch expected found -> "Invalid type: expected " ++ expected ++ ", found " ++ show found
        BadSpecialForm form -> "Unrecognized special form: " ++ show form
        OtherExcept msg -> msg 

Tok.TokenParser {Tok.parens = parens, Tok.identifier = identifier, Tok.reserved = reserved, Tok.lexeme = lexeme, Tok.reservedOp = reservedOp, Tok.whiteSpace = whiteSpace} = 
    Tok.makeTokenParser schemeDef  

schemeDef :: Tok.GenLanguageDef String () Identity
schemeDef = Lang.emptyDef 
    { Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"
    , Tok.identLetter = digit <|> Tok.identStart schemeDef
    , Tok.reservedNames = ["#t", "#f", "'"]
    }