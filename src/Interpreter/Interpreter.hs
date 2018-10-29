module Interpreter.Interpreter where

import           Prelude                 hiding ( lookup )
import           Data.Comp.Algebra
import           Data.Comp.Sum
import           Data.Comp.Term
import           Data.Comp.Algebra.Combine

import           Control.Monad.State.Extended
import           Data.Map
import           Safe

import           AST
import           Interpreter.Types

-- | Types that can be evaluated to a value using a monadic catamorphism
class (Monad m, Traversable f) => Eval m f where
    -- | The monadic Algebra that can evaluate f
    evalAlgM :: AlgM m f Value

    cataEval :: Term f -> m Value
    cataEval = cataM evalAlgM

-- | Types that can be evaluated to a value using a monadic paramorphism
class (Monad m, Traversable f) => ParaEval m f where
    -- | The monadic R-Algebra that can evaluate f
    evalRAlgM :: RAlgM m f Value

    paraEval :: Term f -> m Value
    paraEval = paraM evalRAlgM

instance (Eval m f, Eval m g) => Eval m (f :+: g) where
    evalAlgM = caseF evalAlgM evalAlgM

instance (Functor f, Functor g, ParaEval m f, ParaEval m g) => ParaEval m (f :+: g) where
    evalRAlgM = rAlgMCombine evalRAlgM evalRAlgM


---------- Expressions ----------
evalExpr :: (MonadEnv s m, MonadRE e m) => DesugaredExpr -> m Value
evalExpr (DesugaredExpr expr) = cataEval expr

instance Monad m => Eval m Literal where
    evalAlgM (IntExpr n) = return $ Integer n
    evalAlgM (FloatExpr n)  = return $ Float n
    evalAlgM (BoolExpr b) = return $ Bool b
    evalAlgM (StringExpr s) = return $ String s

instance (Monad m, MonadEnv s m, MonadRE e m) => Eval m Identifier where
    evalAlgM (IdentifierExpr name) = getName name

instance (MonadEnv s m) => Eval m (FuncExpr DesugaredStmt) where
    evalAlgM (FuncExpr sig body) = return $ Function (fst <$> arguments sig) body

instance (Monad m, MonadEnv s m, MonadRE e m) => Eval m Call where
    evalAlgM (CallExpr callee args) = runFunc callee args

---------- Expressions ----------
runStmt :: (MonadState (Environment Value) m, MonadRE e m) => DesugaredStmt -> m Value
runStmt (DesugaredStmt stmt) = paraM evalRAlgM stmt

instance (MonadEnv s m, MonadRE e m) => ParaEval m (SimpleAssignment DesugaredExpr) where
    evalRAlgM (SimpleAssignStmt name expr) = Unit <$ (evalExpr expr >>= bind name)

instance (MonadEnv s m, MonadRE e m) => ParaEval m (ExprStmt DesugaredExpr) where
    evalRAlgM (ExprStmt expr) = evalExpr expr

instance (MonadEnv s m, MonadRE e m) => ParaEval m (WhileLoop DesugaredExpr) where
    evalRAlgM loop@(WhileStmt cond (Block body)) = evalExpr cond >>= \case
        (Bool True) -> runBlock injectedBody  >> evalRAlgM loop
        (Bool False) -> return Unit
        _ -> runtimeError
        where
            injectedBody = Block (DesugaredStmt <$> (deepInject . fst <$> body))

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
