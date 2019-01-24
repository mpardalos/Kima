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
    v            -> throwError (WrongConditionType v)
runStmt (If IfStmt { cond, ifBlk, elseBlk }) = evalExpr cond >>= \case
    (Bool True ) -> runStmt ifBlk
    (Bool False) -> runStmt elseBlk
    v            -> throwError (WrongConditionType v)

runFunc :: MonadInterpreter m => Value -> [Value] -> m Value
runFunc (Function argNames body) args = withState (<> argEnv) (runStmt body)
  where
    argEnv :: Environment Value
    argEnv = Environment $ Map.fromList (zip argNames args)
runFunc (BuiltinFunction f) args                 = f args
runFunc v _ = throwError (NotAFunction v)

bind :: (MonadEnv m) => RuntimeName -> Value -> m ()
bind name val = modify (over Environment $ Map.insert name val)

getName :: (MonadEnv m, MonadRE m) => RuntimeName -> m Value
getName name = gets (Map.lookup name . unEnv) >>= \case
    Just val -> return val
    Nothing  -> throwError (NotInScope name)

-- | Bind either a function or the constructor and accessors of a
-- | DataDef
bindTopLevel :: MonadInterpreter m => RuntimeAST 'TopLevel -> m Value
bindTopLevel (FuncDef name signature body) = do
    let result = Function signature body 
    bind name result
    return result
bindTopLevel (DataDef name members)       = do
    let constructor = BuiltinFunction (return . ProductData)
    forM_ (zip [0..] members) $ \(i, accessorName) ->
        bind accessorName $ BuiltinFunction $ \case
            [ProductData vals] -> case vals `atMay` i of
                Just v -> return v
                Nothing -> throwError (BuiltinFunctionError (show accessorName <> " failed"))
            v -> throwError (BuiltinFunctionError (
                    "Can't use accessor " <> show accessorName <> " on " <> show v))
    bind name constructor
    return constructor

runProgram :: MonadInterpreter m => RuntimeProgram -> m ()
runProgram (Program defs) = do
    forM_ defs bindTopLevel
    mainFunc <- getName (TypedName "main" (KFunc ([] $-> KUnit)))
    _        <- runFunc mainFunc []
    return ()
