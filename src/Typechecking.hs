module Typechecking(
    module Typechecking.Types,
    checkStmt,
    checkExpr,
    checkProgram,
    KTypeM,
    runTypeChecking
) where

import Typechecking.Check
import Typechecking.Monad
import Typechecking.Types