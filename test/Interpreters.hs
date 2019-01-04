module Interpreters where

import           Kima.Interpreter.Types
import           Kima.Interpreter.Interpreter
import           Kima.Typechecking
import           Kima.Typechecking.ConstraintGen
import           Kima.Builtins
import           Kima.AST
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Writer

newtype TestInterpreter a = MockInterpreter {
        runInterpreter
                :: StateT (Environment Value) (
                   Either RuntimeError) a
} deriving (Functor, Applicative, Monad, MonadError RuntimeError, MonadState (Environment Value))

runInTestInterpreter :: RuntimeAST p -> Either RuntimeError Value
runInTestInterpreter = (`evalStateT` baseEnv) . runInterpreter . \case
        ProgramAST ast  -> runProgram ast *> pure Unit
        FuncDefAST func -> pure (evalFuncDef func)
        StmtAST    stmt -> runStmt stmt
        ExprAST    expr -> evalExpr expr

-- | Always returns "test" on read and ignores output
instance MonadConsole TestInterpreter where
        consoleRead = return "test"
        consoleWrite = const (pure ())

constraintsFor :: AnnotatedTVarAST p -> EqConstraintSet
constraintsFor = execWriter . \case 
        ProgramAST ast  -> writeProgramConstraints ast
        FuncDefAST func -> writeFuncDefConstraints func
        StmtAST    stmt -> stmtReturnTVar stmt *> pure ()
        ExprAST    expr -> exprTVar expr *> pure ()

domainsFor :: AnnotatedTVarAST p -> Either TypecheckingError Domains
domainsFor = makeDomains