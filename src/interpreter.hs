import qualified Control.Exception as Exc
import Control.Monad.Except
import Control.Monad.Reader
import System.Environment
import System.Directory
import qualified Data.Map as M
import LispData
import Primitives
import Evaluator
import Parser

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
                    s <- readFile $ args !! 0
                    ((runExceptT $ interp s) >>= (putStrLn . showResult)) `Exc.catch` handler

interp :: String -> ExceptT LispExcept IO LispVal
interp input = either 
        (throwError . ParseExcept) 
        (\x -> (runReaderT (eval x) primEnv))
        (parseExpr input)

handler = (\(Exc.SomeException e) -> putStrLn $ "RuntimeError: " ++ show e)

showResult =  either show show