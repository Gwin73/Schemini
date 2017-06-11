import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (spaces)

import Control.Monad (forM)
 
main = forM (  ["if", "define", "set!", "lambda", "quote" , "(if)", "(if define set!)", "\'if", "\'(if)"] 
    ++ ["#t", "#f", "(#t)" ,"(#t #f #f)", "(#t (#f #f))", "\'#t", "\'(#t #f #f)"] 
    ++ ["123", "()", "(1)" ,"(1 2 3)", "(1 (2 3))", "\'1", "\'(1 2 3)"] 
    ++ ["\"asd\"", "\"\\\"asd\\\"\"", "\"\\\\asd\\\\\"", "(\"asd\")" ,"(\"asd\" \"asd\" \"asd\")", "(\"asd\" (\"asd\" \"asd\"))", "\'\"asd\"", "\'(\"asd\" \"asd\" \"asd\")"]) 
    (putStrLn . show . parseExpr)

parseExpr :: String -> String
parseExpr input = case parse expr "Scheme" input of 
    Left err -> show err
    Right val -> show val

expr :: Parser LispVal
expr 
      =  symbol      
     <|> bool
     <|> integer
     <|> string'
     <|> quoted
     <|> list

symbol :: Parser LispVal
symbol = identifier >>= (return . Symbol)

bool :: Parser LispVal
bool = (reserved "#t" >> (return $ Bool True)) <|> (reserved "#f" >> (return $ Bool False))

integer :: Parser LispVal
integer = lexeme $ many1 digit >>= return . Integer . read

string' :: Parser LispVal
string' = lexeme $ between (char '\"') (char '\"') (many (escapeChar <|> noneOf ['\"', '\\'])) >>= (return . String)
    where escapeChar = char '\\' >> oneOf ['\\', '"'] >>= return

quoted :: Parser LispVal
quoted = lexeme $ string "\'" >> expr >>= \x -> return $ List [Symbol "quote", x]

list :: Parser LispVal
list = parens $ many expr >>= return . List


data LispVal 
    = Symbol String
    | Bool Bool
    | Integer Integer
    | String String
    | List [LispVal]

instance Show LispVal where
    show v = case v of
        Symbol s -> "Symbol: " ++ s
        Bool True -> "Bool: #t"
        Bool False -> "Bool: #f"
        Integer n -> "Integer: " ++ show n
        String s -> "String: " ++ "\"" ++ s ++ "\""
        List l -> "List: " ++ "(" ++ (unwords . map show) l ++ ")"          

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser schemeDef

Tok.TokenParser {Tok.parens = parens, Tok.identifier = identifier, Tok.reserved = reserved, Tok.lexeme = lexeme, Tok.reservedOp = reservedOp} = 
    Tok.makeTokenParser schemeDef  

schemeDef :: Tok.GenLanguageDef String () Identity
schemeDef = Lang.emptyDef 
    { Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"
    , Tok.identLetter = digit <|> Tok.identStart schemeDef
    , Tok.reservedNames = ["#t", "#f", "\'"]
    }