module Interface.Runners where

import AST
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Newtype.Generics
import Data.Bifunctor
import Interface.Types
import System.IO
import Text.Megaparsec
import qualified Frontend as F
import qualified Interpreter as I
import qualified Typechecking as T

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
    runResult <- liftIO (I.runProgram (over Program (fmap @[] desugarFuncDef) src))
    runEither runResult

userThrow :: (MonadInterface m, UserThrowable err) => err -> m a
userThrow = throwError . UserThrowableError

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither = liftEither . bimap UserThrowableError id 
