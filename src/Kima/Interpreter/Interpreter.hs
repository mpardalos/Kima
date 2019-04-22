module Kima.Interpreter.Interpreter where

import           Prelude                 hiding ( lookup )

import           Data.Foldable
import           Control.Newtype.Generics
import           Control.Monad.Except

import           Kima.AST
import           Kima.Control.Monad.State.Extended
import           Kima.Interpreter.Types
import           Kima.KimaTypes

import           Safe
import qualified Data.Map                      as Map

runAST :: MonadInterpreter m => RuntimeAST p -> m Value
runAST (ProgramAST ast)  = Unit <$ runProgram ast
runAST (TopLevelAST ast) = bindTopLevel ast
runAST (StmtAST    ast)  = runStmt ast
runAST (ExprAST    ast)  = evalExpr ast

---------- Expressions ----------
evalExpr :: (MonadInterpreter m) => RuntimeAST 'Expr -> m Value
evalExpr (LiteralE   l     )     = return $ evalLiteral l
evalExpr (IdentifierE name )     = getName name
evalExpr (FuncExpr args _rt body) = return $ Function (uncurry TIdentifier <$> args) body
evalExpr (Call callee args) =
    join (runFunc <$> evalExpr callee <*> (evalExpr `mapM` args))

evalLiteral :: Literal -> Value
evalLiteral (IntExpr    i) = Integer i
evalLiteral (FloatExpr  f) = Float f
evalLiteral (BoolExpr   b) = Bool b
evalLiteral (StringExpr s) = String s

---------- Statements ----------
runStmt :: forall m. MonadInterpreter m => RuntimeAST 'Stmt -> m Value
runStmt (Block stmts) = do
    vals <- runStmt `mapM` stmts
    return (lastDef Unit vals)
runStmt (Assign (WriteAccess name []) expr) = Unit <$ (evalExpr expr >>= bind name)
runStmt (Assign (WriteAccess name field) expr) = do
    newVal <- evalExpr expr
    accessors <- lookupFields (nameType name) field --(getName . \(TName n t) -> TAccessor n t) field
    modifyField name accessors newVal
    return Unit
    where
        modifyField :: IdentifierLike ident => ident ('Annotation KType) -> [Value] -> Value -> m ()
        modifyField baseName subfields newVal = do
            oldVal <- getName baseName
            bind baseName (makeModified oldVal subfields newVal)

        lookupFields :: KType -> [AnnotatedName ('Annotation KType)] -> m [Value]
        lookupFields _ []            = pure []
        lookupFields base (TName subName subType:subs) = do
            sub' <- getName $ TAccessor subName (KFunc ([base] $-> subType))
            (sub':) <$> lookupFields subType subs

        makeModified :: Value -> [Value] -> Value -> Value
        makeModified (ProductData subvals) (AccessorIdx _ idx:accessors) newVal =
            ProductData (update idx subvals (makeModified (subvals !! idx) accessors newVal))
        makeModified _ [] newVal = newVal
        makeModified _ (AccessorIdx fieldName _:_) _ = error ("Tried to access " ++ fieldName ++ " in non-product data")
        makeModified _ (val:_) _ = error ("Tried to use " ++ show val ++ " as an accessor")

        update :: Int -> [a] -> a -> [a]
        update _ []     _ = []
        update 0 (_:xs) y = y:xs
        update n (x:xs) y = x:update (n-1) xs y

runStmt (Let    name t expr) = Unit <$ (evalExpr expr >>= bind (TIdentifier name t))
runStmt (Var    name t expr) = Unit <$ (evalExpr expr >>= bind (TIdentifier name t))
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
runFunc (AccessorIdx memberName idx) [ProductData vals] = case vals `atMay` idx of
    Just v -> return v
    Nothing -> throwError (BuiltinFunctionError (show memberName <> " failed"))
runFunc (AccessorIdx memberName _) args = throwError (BuiltinFunctionError (
    "Can't use accessor " <> show memberName <> " on " <> show args))
runFunc v _ = throwError (NotAFunction v)

bind :: (MonadEnv m, IdentifierLike ident) => ident ('Annotation KType) -> Value -> m ()
bind name val = modify (over Environment $ Map.insert (toIdentifier name) val)

getName :: (MonadEnv m, MonadRE m, IdentifierLike ident) => ident ('Annotation KType) -> m Value
getName name = gets (Map.lookup (toIdentifier name) . unEnv) >>= \case
    Just val -> return val
    Nothing  -> throwError (NotInScope (toIdentifier name))

-- | Bind either a function or the constructor and accessors of a
-- | DataDef
bindTopLevel :: MonadInterpreter m => RuntimeAST 'TopLevel -> m Value
bindTopLevel (FuncDef name args rt body) = do
    let funcType = KFunc ((snd <$> args) $-> rt)
    let function = Function (uncurry TIdentifier <$> args) body
    bind (TIdentifier name funcType) function
    return function
bindTopLevel (DataDef name members)       = do
    let declaredType = KUserType name members
    let memberTypes = snd <$> members
    let constructor = BuiltinFunction (return . ProductData)
    let constructorType = KFunc (memberTypes $-> declaredType)

    forM_ (zip [0..] members) $ \(i, (memberName, memberType)) ->
        let accessorType = KFunc ([declaredType] $-> memberType) in
        bind (TAccessor memberName accessorType) (AccessorIdx memberName i)
    bind (TIdentifier name constructorType) constructor
    return constructor

runProgram :: MonadInterpreter m => RuntimeProgram -> m ()
runProgram (Program defs) = do
    forM_ defs bindTopLevel
    mainFunc <- getName (TIdentifier "main" (KFunc ([] $-> KUnit)))
    _        <- runFunc mainFunc []
    return ()
