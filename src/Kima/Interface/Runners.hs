module Kima.Interface.Runners where

import Control.Monad.Except
import Data.Bifunctor

import Kima.AST.Parsed as Parsed
import Kima.AST.Typed as Typed
import Kima.AST.Desugared as Desugared
import Kima.Desugar
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
parseFile' :: MonadInterface m => FilePath -> m Parsed.Program
parseFile' fn = do 
    src <- liftIO (readFile fn)
    runEither (F.runParser F.program fn src)

typecheckFile = runMonadInterface . (parseFile' >=> typecheckAST')
typecheckAST = runMonadInterface . typecheckAST'
typecheckAST' :: MonadInterface m => Parsed.Program -> m Typed.Program
typecheckAST' = runEither . T.runTypeChecking T.baseCtx . T.checkProgram

desugarFile = runMonadInterface . (parseFile' >=> typecheckAST' >=> desugarAST')
desugarAST = runMonadInterface . desugarAST'
desugarAST' :: MonadInterface m => Typed.Program -> m Desugared.Program
desugarAST' = return . desugarProgram

runFile = runMonadInterface . (parseFile' >=> typecheckAST' >=> desugarAST' >=> runAST')
runAST = runMonadInterface . runAST'
runAST' :: MonadInterface m => Desugared.Program -> m ()
runAST' src = liftIO (I.runProgram src) >>= runEither 

userThrow :: (MonadInterface m, UserThrowable err) => err -> m a
userThrow = throwError . UserThrowableError

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither = liftEither . bimap UserThrowableError id 
