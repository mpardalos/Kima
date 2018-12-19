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

constraintFile = runMonadInterface . (parseFile' >=> desugarAST' >=> constraintAST')
constraintAST = runMonadInterface . constraintAST'
constraintAST' :: MonadInterface m => DesugaredProgram -> m (T.TVarProgram, T.ConstraintSet)
constraintAST' = runMaybe (CustomError "Couldn't resolve type expressions") . T.makeConstraints

typecheckFile = runMonadInterface . (parseFile' >=> desugarAST' >=> typecheckAST')
typecheckAST = runMonadInterface . typecheckAST'
typecheckAST' :: MonadInterface m => DesugaredProgram -> m TypedProgram
typecheckAST' = _ --runEither . _typecheck --T.runTypeChecking T.baseCtx . T.check

desugarFile = runMonadInterface . (parseFile' >=> desugarAST')
desugarAST = runMonadInterface . desugarAST'
desugarAST' :: MonadInterface m => ParsedProgram -> m DesugaredProgram
desugarAST' = return . desugar

runFile = runMonadInterface . (parseFile' >=> desugarAST' >=> typecheckAST' >=> runAST')
runAST = runMonadInterface . runAST'
runAST' :: MonadInterface m => TypedProgram -> m ()
runAST' src = liftIO (I.runProgram src) >>= runEither 

userThrow :: (MonadInterface m, UserThrowable err) => err -> m a
userThrow = throwError . UserThrowableError

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither = liftEither . bimap UserThrowableError id 

runMaybe :: (UserThrowable err, MonadInterface m) => err -> Maybe a -> m a
runMaybe _   (Just a) = pure a 
runMaybe err Nothing  = userThrow err
