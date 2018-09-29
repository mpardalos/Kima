module Debug where

import Control.Monad.State

import qualified Typechecking as T
import qualified Frontend as P

evalChecker :: (a -> T.KTypeM T.KType) -> a -> Either T.TypeError T.KType
evalChecker f = f . (`evalStateT` mempty) . T.runKTypeM

evalCheckExpr  :: P.Expr  -> Either T.TypeError T.KType
evalCheckStmt  :: P.Stmt  -> Either T.TypeError T.KType
evalCheckBlock :: P.Block -> Either T.TypeError T.KType
evalCheckExpr  = evalChecker T.checkExpr
evalCheckStmt  = evalChecker T.checkStmt
evalCheckBlock = evalChecker T.checkBlock