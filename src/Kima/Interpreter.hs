module Kima.Interpreter (
    Kima.Interpreter.run,
    Kima.Interpreter.runWithEnv,
    E.runAST,
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

import Kima.AST

run :: Environment Value -> RuntimeAST p -> IO (Either RuntimeError Value)
run env (ProgramAST  ast) = fmap (const Unit) <$> execInterpreter env (E.runProgram ast)
run _   (TopLevelAST _  ) = pure (Right Unit)
run env (StmtAST     ast) = execInterpreter env (E.runStmt ast)
run env (ExprAST     ast) = execInterpreter env (E.evalExpr ast)

runWithEnv :: Environment Value -> RuntimeAST p -> IO (Either RuntimeError (Value, Environment Value))
runWithEnv env ast  = runInterpreter env (runAST ast)
