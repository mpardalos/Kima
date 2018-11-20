module Kima.Interpreter (
    Kima.Interpreter.runProgram,
    E.evalExpr,
    E.runStmt,
    E.execInterpreter,
    E.RuntimeError(..)
) where

import Kima.Interpreter.Interpreter as E
import Kima.Interpreter.Types as E
import Kima.Interpreter.Monad as E

import Kima.AST

runProgram :: RuntimeAST 'TopLevel -> IO (Either RuntimeError ())
runProgram = execInterpreter . E.runProgram 