module Kima.Interpreter (
    Kima.Interpreter.run,
    Kima.Interpreter.runWithEnv,
    E.evalExpr,
    E.runStmt,
    E.execInterpreter,
    E.MonadInterpreter,
    E.Environment,
    E.Value,
    E.RuntimeError(..)
) where

import Kima.Interpreter.Interpreter as E
import Kima.Interpreter.Types as E
import Kima.Interpreter.Monad as E

import Data.Bifunctor

import Kima.AST

run :: Environment Value -> RuntimeAST p -> IO (Either RuntimeError Value)
run env (ProgramAST  ast) = fmap (const Unit) <$> execInterpreter env (E.runProgram ast)
run _   (TopLevelAST _  ) = pure (Right Unit)
run env (StmtAST     ast) = execInterpreter env (E.runStmt ast)
run env (ExprAST     ast) = execInterpreter env (E.evalExpr ast)

runWithEnv :: Environment Value -> RuntimeAST p -> IO (Either RuntimeError (Value, Environment Value))
runWithEnv env (ProgramAST ast)  = fmap (first (const Unit)) <$> runInterpreter env (E.runProgram ast)
runWithEnv env (TopLevelAST ast) = runInterpreter env (bindTopLevel ast)
runWithEnv env (StmtAST    ast)  = runInterpreter env (E.runStmt ast)
runWithEnv env (ExprAST    ast)  = runInterpreter env (E.evalExpr ast)
