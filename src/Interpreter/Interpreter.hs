module Interpreter.Interpreter where

import Prelude hiding ( lookup )
import Safe

import AST

import Control.Monad.State.Extended

import Data.Comp.Algebra
import Data.Comp.Sum
import Data.Comp.Term
import Data.Map

import Interpreter.Types

-- | Types that can be evaluated to a value using a monadic catamorphism
class (Monad m, Traversable f) => Eval m f where
    -- | The monadic Algebra that can evaluate f
    evalAlgM :: AlgM m f Value

    cataEval :: Term f -> m Value
    cataEval = cataM evalAlgM

-- | Types that can be recursively run inside a monad
-- | Here we use a plain algebra (f (m Value) -> m Value) instead
-- | of a monadic algebra (f Value -> m Value) so that we can control 
-- | the sequencing of operations. This allows for control flow.
class (Monad m, Functor f) => Run m f where
    -- | The Algebra that can run f
    runAlg :: Alg f (m Value)

    run :: Term f -> m Value
    run = cata runAlg

instance (Eval m f, Eval m g) => Eval m (f :+: g) where
    evalAlgM = caseF evalAlgM evalAlgM

instance (Functor f, Functor g, Run m f, Run m g) => Run m (f :+: g) where
    runAlg = caseF runAlg runAlg


---------- Expressions ----------
evalExpr :: (MonadInterpreter m) => DesugaredExpr -> m Value
evalExpr (DesugaredExpr expr) = cataEval expr

instance Monad m => Eval m Literal where
    evalAlgM (IntExpr n) = return $ Integer n
    evalAlgM (FloatExpr n)  = return $ Float n
    evalAlgM (BoolExpr b) = return $ Bool b
    evalAlgM (StringExpr s) = return $ String s

instance (Monad m, MonadEnv m, MonadRE m) => Eval m Identifier where
    evalAlgM (IdentifierExpr name) = getName name

instance (MonadEnv m) => Eval m (FuncExpr DesugaredStmt) where
    evalAlgM (FuncExpr sig body) = return $ Function (fst <$> arguments sig) body

instance (MonadInterpreter m) => Eval m Call where
    evalAlgM (CallExpr (BuiltinFunction1 f) [arg]) = f arg
    evalAlgM (CallExpr (BuiltinFunction2 f) [arg1, arg2]) = f arg1 arg2
    evalAlgM (CallExpr (BuiltinFunction3 f) [arg1, arg2, arg3]) = f arg1 arg2 arg3
    evalAlgM (CallExpr callee args) = runFunc callee args

---------- Expressions ----------
runStmt :: MonadInterpreter m => DesugaredStmt -> m Value
runStmt (DesugaredStmt stmt) = run stmt

instance (Monad m, MonadEnv m, MonadRE m) => Run m BlockStmt where
    runAlg (BlockStmt stmts) = do 
        vals <- sequence stmts
        return (lastDef Unit vals)

instance MonadInterpreter m => Run m (SimpleAssignment DesugaredExpr) where
    runAlg (SimpleAssignStmt name expr) = Unit <$ (evalExpr expr >>= bind name)

instance MonadInterpreter m => Run m (ExprStmt DesugaredExpr) where
    runAlg (ExprStmt expr) = evalExpr expr

-- You just need a plain (non-monadic) catamorphism!
-- You can then do the sequencing yourself
instance MonadInterpreter m => Run m (WhileLoop DesugaredExpr) where
    runAlg loop@(WhileStmt cond body) = evalExpr cond >>= \case
        (Bool True) -> do
            _ <- body
            runAlg loop
        (Bool False) -> return Unit
        _ -> runtimeError

instance MonadInterpreter m => Run m (IfStmt DesugaredExpr) where
    runAlg (IfStmt cond ifblk elseblk) = evalExpr cond >>= \case
        (Bool True) -> ifblk
        (Bool False) -> elseblk
        _ -> runtimeError

runFunc :: MonadInterpreter m => Value -> [Value] -> m Value
runFunc (Function argNames (DesugaredStmt body)) args = withState (<> argEnv) (run body)
  where
    argEnv :: Environment Value
    argEnv = Environment $ fromList (zip argNames args)
runFunc (BuiltinFunction1 f) [arg] = f arg
runFunc (BuiltinFunction2 f) [arg1, arg2] = f arg1 arg2
runFunc (BuiltinFunction3 f) [arg1, arg2, arg3] = f arg1 arg2 arg3
runFunc _ _ = runtimeError

bind :: (MonadEnv m) => Name -> Value -> m ()
bind name val = modify (Environment . insert name val . unEnv) 

getName :: (MonadEnv m, MonadRE m) => Name -> m Value
getName name = gets (lookup name . unEnv) >>= \case
    Just val -> return val
    Nothing  -> runtimeError

evalFuncDef :: MonadEnv m => FuncDef DesugaredStmt -> m Value
evalFuncDef FuncDef { signature, body } =
    return $ Function (fst <$> arguments signature) body

bindFuncDef :: MonadEnv m => FuncDef DesugaredStmt -> m ()
bindFuncDef f = bind (name f) =<< evalFuncDef f

runProgram :: MonadInterpreter m => Program DesugaredStmt -> m ()
runProgram (Program functions) = do
    mapM_ bindFuncDef functions -- Bind all top-level functions
    mainFunc <- getName "main"
    _ <- runFunc mainFunc [] 
    return ()
