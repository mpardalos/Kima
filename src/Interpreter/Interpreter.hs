module Interpreter.Interpreter where

import           Prelude                 hiding ( lookup )
import           Data.Comp.Algebra
import           Data.Comp.Sum

import           Control.Monad.State.Extended
import           Data.Map
import           Safe

import           AST
import           Interpreter.Types

class Eval m f where
    evalAlgM :: EvalAlgM m f

instance (Eval m f, Eval m g) => Eval m (f :+: g) where
    evalAlgM = caseF evalAlgM evalAlgM

---------- Expressions ----------
evalExpr :: (MonadEnv s m, MonadRE e m) => DesugaredExpr -> m Value
evalExpr (DesugaredExpr expr) = cataM evalAlgM expr

instance Monad m => Eval m Literal where
    evalAlgM (IntExpr n) = return $ Integer n
    evalAlgM (FloatExpr n)  = return $ Float n
    evalAlgM (BoolExpr b) = return $ Bool b
    evalAlgM (StringExpr s) = return $ String s

instance (MonadEnv s m, MonadRE e m) => Eval m Identifier where
    evalAlgM (IdentifierExpr name) = getName name

instance (MonadEnv s m) => Eval m (FuncExpr DesugaredStmt) where
    evalAlgM (FuncExpr sig body) = return $ Function (fst <$> arguments sig) body

instance (MonadEnv s m, MonadRE e m) => Eval m Call where
    evalAlgM (CallExpr callee args) = runFunc callee args

---------- Expressions ----------
runStmt :: (MonadState (Environment Value) m, MonadRE e m) => DesugaredStmt -> m Value
runStmt (DesugaredStmt stmt) = cataM evalAlgM stmt

instance (MonadEnv s m, MonadRE e m) => Eval m (SimpleAssignment DesugaredExpr) where
    evalAlgM (SimpleAssignStmt name expr) = Unit <$ (evalExpr expr >>= bind name)

instance (MonadEnv s m, MonadRE e m) => Eval m (ExprStmt DesugaredExpr) where
    evalAlgM (ExprStmt expr) = evalExpr expr

-- TODO: How to make this conditional?
instance (MonadEnv s m, MonadRE e m) => Eval m (WhileLoop DesugaredExpr) where
    evalAlgM (WhileStmt cond body) = evalExpr cond >>= \case
        (Bool True) -> _
        (Bool False) -> _
        _ -> runtimeError
-- runStmt loop@(WhileStmt  cond body) = eval cond >>= \case
--     Bool True  -> runBlock body >> runStmt loop
--     Bool False -> return Nothing
--     _          -> runtimeError

runBlock :: (MonadEnv s m, MonadRE e m) => Block DesugaredStmt -> m Value
runBlock (Block stmts) = lastDef Unit <$> mapM runStmt stmts

runFunc :: (MonadEnv s m, MonadRE e m) => Value -> [Value] -> m Value
runFunc (Function argNames body) args = withState addArgs (runBlock body)
  where
    addArgs :: Environment Value -> Environment Value
    addArgs env = env <> argEnv

    argEnv :: Environment Value
    argEnv = Environment $ fromList (zip argNames args)
runFunc _ _ = runtimeError

bind :: (MonadEnv s m) => Name -> Value -> m ()
bind name val = modify (Environment . insert name val . unEnv) 

getName :: (MonadEnv s m, MonadRE e m) => Name -> m Value
getName name = gets (lookup name . unEnv) >>= \case
    Just val -> return val
    Nothing  -> runtimeError

evalFuncDef :: MonadEnv s m => FuncDef DesugaredStmt -> m Value
evalFuncDef FuncDef { signature, body } =
    return $ Function (fst <$> arguments signature) body

bindFuncDef :: MonadEnv s m => FuncDef DesugaredStmt -> m ()
bindFuncDef f = bind (name f) =<< evalFuncDef f

runProgram :: (MonadEnv s m, MonadRE e m) => Program DesugaredStmt -> m ()
runProgram (Program functions) = do
    mapM_ bindFuncDef functions -- Bind all top-level functions
    mainFunc <- getName "main"
    _ <- runFunc mainFunc [] 
    return ()
