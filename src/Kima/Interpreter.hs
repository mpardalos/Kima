module Kima.Interpreter (
    Kima.Interpreter.run,
    E.evalExpr,
    E.runStmt,
    E.execInterpreter,
    E.MonadInterpreter,
    E.Value,
    E.RuntimeError(..)
) where

import Kima.Interpreter.Interpreter as E
import Kima.Interpreter.Types as E
import Kima.Interpreter.Monad as E

import Data.Functor

import Kima.AST

run :: RuntimeAST p -> IO (Either RuntimeError Value)
run (ProgramAST ast) = execInterpreter (E.runProgram ast) $> Right Unit
run (FuncDefAST ast) = pure (Right (evalFuncDef ast))
run (StmtAST    ast) = execInterpreter (E.runStmt ast)
run (ExprAST    ast) = execInterpreter (E.evalExpr ast)
