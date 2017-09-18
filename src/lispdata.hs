module LispData (LispVal (..), LispExcept(..), Env) where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Except (ExceptT)
import Data.Map (Map(..))

data LispVal 
    = Atom String
    | Bool Bool
    | Int Integer
    | String String
    | List [LispVal]
    | Function ([LispVal] -> Either LispExcept LispVal)
    | IOFunction ([LispVal] -> ExceptT LispExcept IO LispVal)
    | Lambda [String] String LispVal Env
    | Macro [String] String LispVal Env
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
        Lambda s1 s2 s3 s4 -> "#<Lambda>"
        Macro _ _ _ _-> "#<Macro>"
        Alloc -> "#<Nothing>"

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

type Env = Map String LispVal     