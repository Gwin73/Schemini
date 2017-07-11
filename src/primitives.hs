module Primitives (primEnv) where

import Control.Monad.Except
import Data.Map (Map(..), fromList)
import LispData

primEnv = fromList 
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

type Unpacker a = (LispVal -> Either LispExcept a)
type Packer a = (a -> LispVal)   