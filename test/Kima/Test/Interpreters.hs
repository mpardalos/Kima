{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import           Data.Function
import           Data.IORef.Class

newtype TestInterpreter a = MockInterpreter {
        runInterpreter
                :: StateT (Environment (IORef Value)) (
                   ReaderT String (
                   WriterT String (
                   ExceptT RuntimeError
                   IO))) a
}
    deriving newtype (
        Functor, Applicative, Monad,
        MonadError RuntimeError,
        MonadReader String,
        MonadState (Environment (IORef Value)),
        MonadWriter String,
        MonadIO)
    deriving anyclass MonadIORef

runInTestInterpreter :: AST p Runtime -> IO (Either RuntimeError (Value, String))
runInTestInterpreter = runInTestInterpreterWithInput ""

runInTestInterpreterWithInput :: String -> AST p Runtime -> IO (Either RuntimeError (Value, String))
runInTestInterpreterWithInput input inAST = do
    refEnv <- traverse newIORef baseEnv
    inAST & runExceptT
        . runWriterT
        . (`runReaderT` input)
        . (`evalStateT` refEnv)
        . Kima.Test.Interpreters.runInterpreter . \case
            ProgramAST  ast  -> runProgram ast $> Unit
            TopLevelAST ast  -> bindTopLevel ast
            StmtAST     stmt -> runStmt stmt
            ExprAST     expr -> evalExpr expr

instance MonadConsole TestInterpreter where
    consoleRead = ask
    consoleWrite = tell

constraintsFor :: AST p TVars -> EqConstraintSet
constraintsFor = execWriter . \case
        ProgramAST ast  -> writeProgramConstraints ast
        TopLevelAST func -> writeTopLevelConstraints func
        StmtAST    stmt -> stmtReturnTVar stmt $> ()
        ExprAST    expr -> exprTVar expr $> ()
