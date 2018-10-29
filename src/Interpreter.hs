module Interpreter (
    Interpreter.runProgram,
    E.evalExpr,
    E.runStmt,
    E.execInterpreter,
    E.RuntimeError(..)
) where

import Interpreter.Interpreter as E
import Interpreter.Types as E

import AST

runProgram :: Program DesugaredStmt -> IO (Either RuntimeError ())
runProgram = execInterpreter . E.runProgram 