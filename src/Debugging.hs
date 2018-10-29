module Debugging where

import AST
import System.IO
import Text.Megaparsec
import qualified Frontend as F
import qualified Typechecking as T
import qualified Interpreter as I

evalChecker :: (a -> T.KTypeM T.KType) -> a -> Either T.TypeError T.KType
evalChecker f = T.runTypeChecking mempty . f

evalCheckExpr  = evalChecker T.checkExpr
evalCheckStmt  = evalChecker T.checkStmt

parseBlock = either (putStrLn . F.parseErrorPretty) print . F.runParser F.block ""
parseStmt = either (putStrLn . F.parseErrorPretty) print . F.runParser F.stmt ""
parseExpr = either (putStrLn . F.parseErrorPretty) print . F.runParser F.expr ""

parseRepl :: IO ()
parseRepl = do 
    hSetBuffering stdin LineBuffering 
    line <- putStr "> " >> getLine
    let res = F.runParser (foldl1 (<|>) (try <$> [show <$> F.expr, show <$> F.stmt, show <$> F.block])) "" line
    either (putStrLn . F.parseErrorPretty) putStrLn res
    parseRepl

parseFile :: FilePath -> IO (Either F.ParseError (Program Stmt))
parseFile fn = do 
    src <- readFile fn
    return $ F.runParser F.program fn src

typecheckFile :: FilePath -> IO (Either T.TypeError ())
typecheckFile fn = do
    (Right src) <- parseFile fn
    return $ T.runTypeChecking mempty (T.checkProgram src)

runFile :: FilePath -> IO (Either I.RuntimeError ())
runFile fn = do
    (Right src) <- parseFile fn
    (Right ()) <- typecheckFile fn
    I.runProgram (_desugar src)