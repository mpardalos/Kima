module Debug where

import Control.Monad.State

import qualified Typechecking.Check as T
import qualified Typechecking.BaseTypes as T
import qualified Frontend.BaseTypes as P

evalChecker :: (a -> T.KTypeM T.KType) -> a -> Either T.TypeError T.KType
evalChecker = (.) $ (`evalStateT` mempty) . T.runKTypeM

evalCheckExpr  :: P.Expr  -> Either T.TypeError T.KType
evalCheckStmt  :: P.Stmt  -> Either T.TypeError T.KType
evalCheckBlock :: P.Block -> Either T.TypeError T.KType
evalCheckExpr  = evalChecker T.checkExpr
evalCheckStmt  = evalChecker T.checkStmt
evalCheckBlock = evalChecker T.checkBlock