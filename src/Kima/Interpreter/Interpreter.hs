module Kima.Interpreter.Interpreter where

import           Prelude                 hiding ( lookup )

import           Control.Newtype.Generics
import           Control.Monad.Except

import           Kima.AST
import           Kima.Control.Monad.State.Extended
import           Kima.Interpreter.Types
import           Kima.KimaTypes

import           Safe
import qualified Data.Map                      as Map

---------- Expressions ----------
evalExpr :: (MonadInterpreter m) => RuntimeAST 'Expr -> m Value
evalExpr (LiteralE   l     ) = return $ evalLiteral l
evalExpr (Identifier name  ) = getName name
evalExpr (FuncExpr sig body) = return $ Function sig body
evalExpr (Call callee args) =
    join (runFunc <$> evalExpr callee <*> (evalExpr `mapM` args))

evalLiteral :: Literal -> Value
evalLiteral (IntExpr    i) = Integer i
evalLiteral (FloatExpr  f) = Float f
evalLiteral (BoolExpr   b) = Bool b
evalLiteral (StringExpr s) = String s

---------- Statements ----------
runStmt :: MonadInterpreter m => RuntimeAST 'Stmt -> m Value
runStmt (Block stmts) = do
    vals <- runStmt `mapM` stmts
    return (lastDef Unit vals)
runStmt (Assign name expr) = Unit <$ (evalExpr expr >>= bind name)
runStmt (ExprStmt expr) = evalExpr expr
runStmt loop@(While WhileStmt { cond, body }) = evalExpr cond >>= \case
    (Bool True ) -> runStmt body *> runStmt loop
    (Bool False) -> return Unit
    _            -> throwError WrongConditionType
runStmt (If IfStmt { cond, ifBlk, elseBlk }) = evalExpr cond >>= \case
    (Bool True ) -> runStmt ifBlk
    (Bool False) -> runStmt elseBlk
    _            -> throwError WrongConditionType

runFunc :: MonadInterpreter m => Value -> [Value] -> m Value
runFunc (Function argNames body) args = withState (<> argEnv) (runStmt body)
  where
    argEnv :: Environment Value
    argEnv = Environment $ Map.fromList (zip argNames args)
runFunc (BuiltinFunction0 f) []                 = f 
runFunc (BuiltinFunction1 f) [arg]              = f arg
runFunc (BuiltinFunction2 f) [arg1, arg2]       = f arg1 arg2
runFunc (BuiltinFunction3 f) [arg1, arg2, arg3] = f arg1 arg2 arg3

runFunc BuiltinFunction0{} (length -> argCount) =
    throwError (WrongArgumentCount 0 argCount)
runFunc BuiltinFunction1{} (length -> argCount) =
    throwError (WrongArgumentCount 1 argCount)
runFunc BuiltinFunction2{} (length -> argCount) =
    throwError (WrongArgumentCount 2 argCount)
runFunc BuiltinFunction3{} (length -> argCount) =
    throwError (WrongArgumentCount 3 argCount)
runFunc v _ = throwError (NotAFunction v)

bind :: (MonadEnv m) => RuntimeName -> Value -> m ()
bind name val = modify (over Environment $ Map.insert name val)

getName :: (MonadEnv m, MonadRE m) => RuntimeName -> m Value
getName name = gets (Map.lookup name . unEnv) >>= \case
    Just val -> return val
    Nothing  -> throwError (NotInScope name)

evalFuncDef :: RuntimeAST 'FunctionDef -> Value
evalFuncDef (FuncDef _ signature body) = Function signature body

runProgram :: MonadInterpreter m => RuntimeAST 'TopLevel -> m ()
runProgram (Program functions) = do
    forM_ functions $ \f@(FuncDef name _ _) -> bind name (evalFuncDef f)
    mainFunc <- getName (TypedName "main" (KFunc ([] $-> KUnit)))
    _        <- runFunc mainFunc []
    return ()
