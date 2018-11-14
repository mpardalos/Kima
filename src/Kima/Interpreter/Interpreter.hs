module Kima.Interpreter.Interpreter where

import Debug.Trace

import Prelude hiding ( lookup )

import Control.Newtype.Generics

import Kima.AST.Desugared as Desugared
import Kima.AST.Common hiding (Name)
import Kima.AST.Expression
import Kima.Control.Monad.State.Extended
import Kima.Interpreter.Types
import Kima.KimaTypes

import Safe
import qualified Data.Map as Map

---------- Expressions ----------
evalExpr :: (MonadInterpreter m) => Expr -> m Value
evalExpr (LiteralExpr l) = return $ evalLiteral l
evalExpr (Identifier name) = getName name
evalExpr (FuncExpr sig body) = return $ Function sig body
evalExpr (Call callee args) = runFunc <$> evalExpr callee <*> (evalExpr `mapM` args) >>= id

evalLiteral :: Literal -> Value
evalLiteral (IntExpr i) = Integer i
evalLiteral (FloatExpr f) = Float f
evalLiteral (BoolExpr b) = Bool b
evalLiteral (StringExpr s) = String s

---------- Statements ----------
runStmt :: MonadInterpreter m => Stmt -> m Value
runStmt (BlockStmt stmts) = do 
    vals <- runStmt `mapM` stmts
    return (lastDef Unit vals)
runStmt (AssignStmt name expr) = Unit <$ (evalExpr expr >>= bind name)
runStmt (ExprStmt expr) = evalExpr expr
runStmt loop@(WhileStmt cond body) = evalExpr cond >>= \case
    (Bool True) -> do
        _ <- runStmt body
        runStmt loop
    (Bool False) -> return Unit
    _ -> runtimeError
runStmt (IfStmt cond ifblk elseblk) = evalExpr cond >>= \case
    (Bool True) -> runStmt ifblk
    (Bool False) -> runStmt elseblk
    _ -> runtimeError

runFunc :: MonadInterpreter m => Value -> [Value] -> m Value
runFunc (Function argNames body) args = withState (<> argEnv) (runStmt body)
  where
    argEnv :: Environment Value
    argEnv = Environment $ Map.fromList (zip argNames args)
runFunc (BuiltinFunction1 f) [arg] = f arg
runFunc (BuiltinFunction2 f) [arg1, arg2] = f arg1 arg2
runFunc (BuiltinFunction3 f) [arg1, arg2, arg3] = f arg1 arg2 arg3
runFunc _ _ = runtimeError

bind :: (MonadEnv m) => RuntimeName -> Value -> m ()
bind name val = modify (over Environment $ Map.insert name val) 

getName :: (MonadEnv m, MonadRE m) => RuntimeName -> m Value
getName name = gets (Map.lookup name . unEnv) >>= \case
    Just val -> return val
    Nothing  -> traceShow name runtimeError

evalFuncDef :: Desugared.FuncDef -> Value
evalFuncDef FuncDef { signature, body } = Function signature body

runProgram :: MonadInterpreter m => Desugared.Program -> m ()
runProgram (Program functions) = do
    forM_ functions $ \f -> bind (name f) (evalFuncDef f)
    mainFunc <- getName (TypedName "main" (KFunc ([] $-> KUnit)))
    _ <- runFunc mainFunc [] 
    return ()
