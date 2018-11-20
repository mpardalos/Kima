module Kima.Interface.Runners where

import Control.Monad.Except
import Data.Bifunctor

import Kima.AST
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
parseFile' :: MonadInterface m => FilePath -> m ParsedProgram
parseFile' fn = do 
    src <- liftIO (readFile fn)
    runEither (F.runParser F.program fn src)

typecheckFile = runMonadInterface . (parseFile' >=> typecheckAST')
typecheckAST = runMonadInterface . typecheckAST'
typecheckAST' :: MonadInterface m => ParsedProgram -> m TypedProgram
typecheckAST' = runEither . T.runTypeChecking T.baseCtx . T.check

desugarFile = runMonadInterface . (parseFile' >=> typecheckAST' >=> desugarAST')
desugarAST = runMonadInterface . desugarAST'
desugarAST' :: MonadInterface m => TypedProgram -> m DesugaredProgram
desugarAST' = return . desugar

disambiguateFile = runMonadInterface . (parseFile' >=> typecheckAST' >=> desugarAST')
disambiguateAST = runMonadInterface . desugarAST'
disambiguateAST' :: MonadInterface m => DesugaredProgram -> m RuntimeProgram
disambiguateAST' = return . _disambiguate

runFile = runMonadInterface . (parseFile' >=> typecheckAST' >=> desugarAST' >=> disambiguateAST' >=> runAST')
runAST = runMonadInterface . runAST'
runAST' :: MonadInterface m => RuntimeProgram -> m ()
runAST' src = liftIO (I.runProgram src) >>= runEither 

userThrow :: (MonadInterface m, UserThrowable err) => err -> m a
userThrow = throwError . UserThrowableError

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither = liftEither . bimap UserThrowableError id 
