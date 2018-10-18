module Interpreter.Interpreter where

import           Prelude                 hiding ( lookup )

import           Control.Monad.State.Extended
import           Data.Map
import           Safe

import           AST
import           Interpreter.Types
import           Interpreter.Builtins

eval :: (MonadEnv s m, MonadRE e m) => Expr -> m Value
eval (IntExpr    n        ) = return $ Integer n
eval (FloatExpr  f        ) = return $ Float f
eval (BoolExpr   b        ) = return $ Bool b
eval (StringExpr s        ) = return $ String s
eval (FuncExpr sig body   ) = return $ Function (fst <$> arguments sig) body
eval (IdentifierExpr name ) = getName name
eval (CallExpr callee args) = runFunc <$> eval callee <*> mapM eval args >>= id
eval (BinExpr op l r      ) = evalBinOp op <$> eval l <*> eval r >>= id
eval (UnaryExpr op e      ) = evalUnaryOp op <$> eval e >>= id

runStmt :: (MonadState (Environment Value) m, MonadRE e m) => Stmt -> m (Maybe Value)
runStmt (     LetStmt name _t expr) = Nothing <$ (eval expr >>= bind name)
runStmt (     VarStmt name _t expr) = Nothing <$ (eval expr >>= bind name)
runStmt (     AssignStmt name expr) = Nothing <$ (eval expr >>= bind name)
runStmt loop@(WhileStmt  cond body) = eval cond >>= \case
    Bool True  -> runBlock body >> runStmt loop
    Bool False -> return Nothing
    _          -> runtimeError
runStmt (ExprStmt expr) = Just <$> eval expr

runBlock :: (MonadEnv s m, MonadRE e m) => Block -> m Value
runBlock (Block stmts) = do
    res <- lastDef Nothing <$> mapM runStmt stmts
    return $ case res of
        Nothing -> Unit
        Just v  -> v

runFunc :: (MonadEnv s m, MonadRE e m) => Value -> [Value] -> m Value
runFunc (Function argNames body) args = withState addArgs (runBlock body)
  where
    addArgs :: Environment Value -> Environment Value
    addArgs env = env <> argEnv

    argEnv :: Environment Value
    argEnv = Environment $ fromList (zip argNames args)
runFunc _ _ = runtimeError

bind :: (MonadState s m, s ~ (Environment Value)) => Name -> Value -> m ()
bind name val = modify (Environment . insert name val . unEnv) 

getName :: (MonadEnv s m, MonadRE e m) => Name -> m Value
getName name = gets (lookup name . unEnv) >>= \case
    Just val -> return val
    Nothing  -> runtimeError

evalFuncDef :: MonadEnv s m => FuncDef -> m Value
evalFuncDef FuncDef { signature, body } =
    return $ Function (fst <$> arguments signature) body

bindFuncDef :: MonadEnv s m => FuncDef -> m ()
bindFuncDef f = bind (name f) =<< evalFuncDef f

runProgram :: (MonadEnv s m, MonadRE e m) => Program -> m ()
runProgram (Program functions) = do
    mapM_ bindFuncDef functions -- Bind all top-level functions
    mainFunc <- getName "main"
    _ <- runFunc mainFunc [] 
    return ()
