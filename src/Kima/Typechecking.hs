module Kima.Typechecking(
    module Kima.Typechecking.Types,
    check,
    KTypeM,
    runTypeChecking,
    builtinTypes, baseCtx
) where

import Kima.Typechecking.Check
import Kima.Typechecking.Monad
import Kima.Typechecking.Types
import Kima.Typechecking.Builtins