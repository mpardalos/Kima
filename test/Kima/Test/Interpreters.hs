module Kima.Test.Interpreters where

import           Kima.Interpreter.Types
import           Kima.Interpreter.Interpreter
import           Kima.Typechecking
import           Kima.Typechecking.ConstraintGen
import           Kima.Builtins
import           Kima.AST
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Functor

newtype TestInterpreter a = MockInterpreter {
        runInterpreter
                :: StateT (Environment Value) (
                   ReaderT String (
                   WriterT String (
                   Either RuntimeError))) a
} deriving (
        Functor, Applicative, Monad, 
        MonadError RuntimeError, 
        MonadReader String,
        MonadState (Environment Value),
        MonadWriter String)

runInTestInterpreter :: RuntimeAST p -> Either RuntimeError (Value, String)
runInTestInterpreter = runInTestInterpreterWithInput ""

runInTestInterpreterWithInput :: String -> RuntimeAST p -> Either RuntimeError (Value, String)
runInTestInterpreterWithInput input = runWriterT . (`runReaderT` input) . (`evalStateT` baseEnv) . runInterpreter . \case
        ProgramAST ast  -> runProgram ast $> Unit
        FuncDefAST func -> pure (evalFuncDef func)
        StmtAST    stmt -> runStmt stmt
        ExprAST    expr -> evalExpr expr

instance MonadConsole TestInterpreter where
        consoleRead = ask
        consoleWrite = tell

constraintsFor :: AnnotatedTVarAST p -> EqConstraintSet
constraintsFor = execWriter . \case 
        ProgramAST ast  -> writeProgramConstraints ast
        FuncDefAST func -> writeFuncDefConstraints func
        StmtAST    stmt -> stmtReturnTVar stmt $> ()
        ExprAST    expr -> exprTVar expr $> ()

domainsFor :: AnnotatedTVarAST p -> Either TypecheckingError Domains
domainsFor = makeDomains