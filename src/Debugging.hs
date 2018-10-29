module Debugging where

import System.IO

import AST
import Text.Megaparsec
import qualified Typechecking as T
import qualified Frontend as F

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

parseFile :: FilePath -> IO ()
parseFile fn = do 
    src <- readFile fn
    case F.runParser F.program fn src of
        Left err -> putStrLn $ F.parseErrorPretty err
        Right (Program ast) -> putStrLn $ show ast

typecheckFile :: FilePath -> IO ()
typecheckFile _ = _

runFile :: FilePath -> IO ()
runFile _ = _