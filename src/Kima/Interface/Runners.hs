module Kima.Interface.Runners where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Newtype.Generics
import Data.Bifunctor

import Kima.AST
import Kima.Interface.Types
import qualified Kima.Frontend as F
import qualified Kima.Interpreter as I
import qualified Kima.Typechecking as T

import System.IO
import Text.Megaparsec

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

parseFile = runMonadInterface . parseFile'
parseFile' :: MonadInterface m => FilePath -> m (Program Stmt)
parseFile' fn = do 
    src <- liftIO (readFile fn)
    runEither (F.runParser F.program fn src)

typecheckFile = runMonadInterface . typecheckFile'
typecheckFile' :: MonadInterface m => FilePath -> m ()
typecheckFile' fn = do
    src <- parseFile' fn
    runEither (T.runTypeChecking T.baseCtx (T.checkProgram src))

runFile = runMonadInterface . runFile'
runFile' :: MonadInterface m => FilePath -> m ()
runFile' fn = do
    src <- parseFile' fn
    typecheckFile' fn
    let desugaredProgram = over Program (fmap @[] desugarFuncDef) src
    runResult <- liftIO (I.runProgram desugaredProgram)
    runEither runResult

userThrow :: (MonadInterface m, UserThrowable err) => err -> m a
userThrow = throwError . UserThrowableError

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither = liftEither . bimap UserThrowableError id 
