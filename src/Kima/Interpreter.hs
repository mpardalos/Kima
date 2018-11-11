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

import Kima.AST.Desugared as Desugared

runProgram :: Desugared.Program -> IO (Either RuntimeError ())
runProgram = execInterpreter . E.runProgram 