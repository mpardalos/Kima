module Kima.Interpreter.Interpreter where

import           Prelude                 hiding ( lookup )

import           Data.Foldable
import           Data.Coerce
import           Data.IORef.Class
import           Control.Monad.Except

import           Kima.AST
import           Control.Monad.State.Extended
import           Kima.Interpreter.Types

import           Safe
import           GHC.Exts
import qualified Data.Map                      as Map

---------- Expressions ----------
evalExpr :: MonadInterpreter m => Expr Runtime -> m Value
evalExpr (LiteralE   l     )     = return $ evalLiteral l
evalExpr (IdentifierE name )     = getName name
evalExpr (FuncExpr args _eff _rt body) = Function (uncurry TIdentifier <$> args) body <$> get
evalExpr (Call callee args) =
    join (runFunc <$> evalExpr callee <*> (evalExpr `mapM` args))

evalLiteral :: Literal -> Value
evalLiteral (IntExpr    i) = Integer i
evalLiteral (FloatExpr  f) = Float f
evalLiteral (BoolExpr   b) = Bool b
evalLiteral (StringExpr s) = String s

---------- Statements ----------
runStmt :: forall m. MonadInterpreter m => Stmt Runtime -> m Value
runStmt (Block stmts) = do
    vals <- runStmt `mapM` stmts
    return (lastDef Unit vals)
runStmt (Assign (WriteAccess name []) expr) = Unit <$ (evalExpr expr >>= bind name)
runStmt (Assign (WriteAccess name path) expr) = do
    oldVal <- getName name
    newVal <- evalExpr expr

    fieldIndices <- fmap fieldIndex <$> lookupFields (nameType name) path
    bind name (modifyField oldVal fieldIndices newVal)

    return Unit
    where
        -- | Change a field inside a value.
        modifyField
            :: Value -- | Original value
            -> [Int] -- | Path to the field, as a list of field indices
            -> Value -- | Value to update the field to
            -> Value
        modifyField (ProductData vals) (field : subFields) newVal = let
            updatedField = modifyField (vals !! field) subFields newVal
            in ProductData (update field vals updatedField)
        modifyField _ [] newVal = newVal
        modifyField _ (n:_) _ = error
            ("Tried to access field " ++ show n ++ " in non-product data")

        -- | Get the accessors for a path (list of fields) given the starting type
        lookupFields
            :: KType -- | Type of the starting value (first value in the path)
            -> [AnnotatedName ('Annotation KType)] -- | Path
            -> m [Value]
        lookupFields baseType (TName subName subType:subFieldNames) = do
            let accessorType = KFunc [baseType] [] subType
            thisField <- getName (TAccessor subName accessorType)
            subFields <- lookupFields subType subFieldNames
            return (thisField:subFields)
        lookupFields _ [] = pure []

        -- | Get the index that an Accessor accesses. Throws an error if the
        -- | value is not an Accessor
        fieldIndex :: Value -> Int
        fieldIndex (AccessorIdx _ n) = n
        fieldIndex val = error (show val ++ " is not an accessor")

        update
            :: Int -- | Index to update
            -> [a] -- | Original list
            -> a -- | New value
            -> [a]
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
runFunc (Function argNames body closure) args = do
    argRefs <- mapM newIORef args
    let argEnv = fromList (zip argNames argRefs)

    -- Order is IMPORTANT. Data.Map's (<>) prefers the left side. So here
    -- arguments take precedence over the closure which takes precedence over
    -- the active environment
    withState ((argEnv <> closure) <>) (runStmt body)
runFunc (BuiltinFunction f) args                 = f args
runFunc (AccessorIdx memberName idx) [ProductData vals] = case vals `atMay` idx of
    Just v -> return v
    Nothing -> throwError (BuiltinFunctionError (show memberName <> " failed"))
runFunc (AccessorIdx memberName _) args = throwError (BuiltinFunctionError (
    "Can't use accessor " <> show memberName <> " on " <> show args))
runFunc v _ = throwError (NotAFunction v)

bind :: (MonadEnv m, IdentifierLike ident) => ident ('Annotation KType) -> Value -> m ()
bind name val = gets (Map.lookup (toIdentifier name) . unEnv) >>= \case
    Just ref -> writeIORef ref val
    Nothing -> do
        valueRef <- newIORef val
        modify (coerce $ Map.insert (toIdentifier name) valueRef)

getName :: (MonadEnv m, MonadRE m, IdentifierLike ident) => ident ('Annotation KType) -> m Value
getName name = gets (Map.lookup (toIdentifier name) . unEnv) >>= \case
    Just ref -> readIORef ref
    Nothing  -> throwError (NotInScope (toIdentifier name))

-- | Bind all the relevant items for a top-level declaration
bindTopLevel :: MonadInterpreter m => TopLevel Runtime -> m ()
bindTopLevel (FuncDef name args eff rt body) = do
    let funcType = KFunc (snd <$> args) eff rt
    let funcIdentifier = TIdentifier name funcType

    -- Bind it initially to something just to create the reference.
    -- Necessary because the closure will include the function itself
    bind funcIdentifier Unit
    closure <- get
    let function = Function (uncurry TIdentifier <$> args) body closure
    -- Then, when we have the function, give the correct binding
    bind funcIdentifier function
bindTopLevel (DataDef name members)       = do
    let declaredType = KUserType name members
    let memberTypes = snd <$> members
    let constructor = BuiltinFunction (return . ProductData)
    let constructorType = KFunc memberTypes [] declaredType

    forM_ (zip [0..] members) $ \(i, (memberName, memberType)) ->
        let accessorType = KFunc [declaredType] [] memberType in
        bind (TAccessor memberName accessorType) (AccessorIdx memberName i)
    bind (TIdentifier name constructorType) constructor
bindTopLevel (OperationDef name args rt)       = do
    let declaredOperation = KOperation name (snd <$> args) rt
    let declaredEffect = [declaredOperation]
    let declaredFunctionType = KFunc (snd <$> args) declaredEffect rt

    -- TODO Bind handler

    return ()


runModule :: MonadInterpreter m => RuntimeIdentifier -> Module Runtime -> m ()
runModule mainName (Program defs) = do
    forM_ defs bindTopLevel
    mainFunc <- getName mainName
    _        <- runFunc mainFunc []
    return ()
