module Parser (parseExpr) where

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Functor.Identity (Identity)
import LispData

parseExpr :: String -> Either ParseError LispVal
parseExpr = parse (whiteSpace >> lexeme expr <* eof) "Schemini"

expr :: Parser LispVal
expr 
    =  atom      
    <|> bool
    <|> int
    <|> str
    <|> quoted
    <|> list

atom :: Parser LispVal
atom = identifier >>= return . Atom

bool :: Parser LispVal
bool = (reserved "#t" >> (return $ Bool True)) <|> (reserved "#f" >> (return $ Bool False))

int :: Parser LispVal
int = lexeme $ many1 digit >>= return . Int . read

str :: Parser LispVal
str = stringLiteral >>= (return . String)

quoted :: Parser LispVal
quoted = lexeme $ string "'" >> expr >>= \x -> return $ List [Atom "quote", x]

list :: Parser LispVal
list = parens $ many expr >>= return . List

Tok.TokenParser {Tok.parens = parens, Tok.identifier = identifier, Tok.reserved = reserved, Tok.lexeme = lexeme, Tok.whiteSpace = whiteSpace, Tok.stringLiteral = stringLiteral} = 
    Tok.makeTokenParser schemeDef  

schemeDef :: Tok.GenLanguageDef String () Identity
schemeDef = Lang.emptyDef 
    { Tok.commentLine = ";"
    , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~+-|"
    , Tok.identLetter = digit <|> Tok.identStart schemeDef
    , Tok.reservedNames = ["#t", "#f"]
    }