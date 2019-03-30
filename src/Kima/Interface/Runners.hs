module Kima.Interface.Runners where

import Control.Monad.Except
import Control.Monad.State

import Kima.AST
import Kima.Builtins
import Kima.Desugar
import Kima.Interface.Types
import qualified Kima.Frontend as F
import qualified Kima.Interpreter as I
import qualified Kima.Typechecking as T

import System.IO
import Text.Megaparsec

parseBlock = F.runParser F.block ""
parseStmt =  F.runParser F.stmt ""
parseExpr =  F.runParser F.expr ""

parseRepl :: IO ()
parseRepl = do 
    hSetBuffering stdin LineBuffering 
    line <- putStr "> " >> getLine
    let res = F.runParser (foldl1 (<|>) (try <$> [show <$> F.expr, show <$> F.stmt, show <$> F.block])) "" line
    either (putStrLn . F.errorBundlePretty) putStrLn res
    parseRepl

parseFile = runMonadInterface . parseFile'
parseFile' :: MonadInterface m => FilePath -> m ParsedProgram
parseFile' fn = do 
    src <- liftIO (readFile fn)
    runEither (F.runParser F.program fn src)

desugarFile = runMonadInterface . (parseFile' >=> desugarAST')
desugarAST = runMonadInterface . desugarAST'
desugarAST' :: MonadInterface m => ParsedAST p -> m (DesugaredAST p)
desugarAST' = return . desugar

tVarAnnotateFile = runMonadInterface . (parseFile' >=> desugarAST' >=> tVarAnnotateAST')
tVarAnnotateAST = runMonadInterface . tVarAnnotateAST'
tVarAnnotateAST' :: MonadInterface m => DesugaredAST p -> m (T.TVarAST p)
tVarAnnotateAST' = runEither . (`evalStateT` baseTypeCtx) . (fmap T.addTVars . T.resolveTypes)

constraintFile = runMonadInterface . (parseFile' >=> desugarAST' >=> tVarAnnotateAST' >=> constraintAST')
constraintAST = runMonadInterface . constraintAST'
constraintAST' :: MonadInterface m => T.TVarAST p -> m T.EqConstraintSet
constraintAST' =  pure . T.makeConstraints

domainsOfFile = runMonadInterface . (parseFile' >=> desugarAST' >=> tVarAnnotateAST' >=> domainsOfAST')
domainsOfAST = runMonadInterface . domainsOfAST'
domainsOfAST' :: MonadInterface m => T.TVarAST p -> m T.Domains
domainsOfAST' =  runEither . T.makeDomains baseTypeCtx

typecheckFile = runMonadInterface . (parseFile' >=> desugarAST' >=> typecheckAST')
typecheckAST = runMonadInterface . typecheckAST'
typecheckAST' :: MonadInterface m => DesugaredAST p -> m (TypedAST p)
typecheckAST' = runEither . T.typecheck baseTypeCtx

runFile = runMonadInterface . (parseFile' >=> desugarAST' >=> typecheckAST' >=> runAST')
runAST = runMonadInterface . runAST'
runAST' :: MonadInterface m => TypedAST p -> m I.Value
runAST' src = liftIO (I.run baseEnv src) >>= runEither
