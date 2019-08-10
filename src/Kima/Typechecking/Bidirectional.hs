{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.Bidirectional
    ( runTypeChecking
    , infer
    , check
    , inferReturns
    , checkReturns
    )
where

-- We use a variant of bidirectional type inference here. The reason for this
-- variation is that
-- a) Our types include *mutability* information
-- b) We have overloading
--
-- Therefore infer gives a *Binding* not a single type. We can, however pass a
-- simple type to check since we just have to check that that type is present in
-- the binding, and the mutability information is only used when checking
-- assignments

import           Safe

import           Control.Monad.State.Extended
import           Control.Monad.Except

import           Data.Maybe
import           Data.Foldable
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Kima.AST
import           Kima.KimaTypes
import           Kima.Typechecking.TypeCtx

type MonadTC m = (MonadState TypeCtx m, MonadError String m)

runTypeChecking ctx action = runExcept (evalStateT action ctx)

---------------------------------
---------- Expressions ----------
---------------------------------

infer :: MonadTC m => AST 'Expr TypeAnnotated -> m Binding
infer (LiteralE    (IntExpr    _)) = pure (Binding Constant [KInt])
infer (LiteralE    (FloatExpr  _)) = pure (Binding Constant [KFloat])
infer (LiteralE    (BoolExpr   _)) = pure (Binding Constant [KBool])
infer (LiteralE    (StringExpr _)) = pure (Binding Constant [KString])
infer (IdentifierE name          ) = Map.lookup name <$> gets bindings >>= \case
    Just binding -> pure binding
    Nothing      -> throwError (show name ++ " is not present in env")
infer (FuncExpr args rt body) = do
    let expectedType = KFunc ((snd <$> args) $-> rt)
    withState (addArgs args) $ checkReturns expectedType body
    return $ Binding Constant [expectedType]
infer (Call callee args) = do
    (Binding _ calleeTypes) <- infer callee
    let calleeFunctionTypes = mapMaybe kTypeSignature (Set.toList calleeTypes)
    assert (not $ null calleeFunctionTypes) "Call to value of non-function type"

    -- Get the possible return types from all of the possible function types
    maybeReturnTypes <- forM calleeFunctionTypes $ \(Signature argTypes rt) ->
        if length argTypes == length args
            then
                zipWithM_ check argTypes args
                >>           pure (Just rt)
                `catchError` (\_ -> pure Nothing)
            else return Nothing
    let returnTypes = catMaybes maybeReturnTypes

    return (Binding Constant (Set.fromList returnTypes))
  where
    kTypeSignature (KFunc sig) = Just sig
    kTypeSignature _           = Nothing

check :: MonadTC m => KType -> AST 'Expr TypeAnnotated -> m ()
check expectedType expr = do
    inferedBinding@(Binding _ inferedTypes) <- infer expr
    assert (expectedType `inBinding` inferedBinding)
           (show expectedType <> " is not one of " <> show inferedTypes)
  where
    inBinding :: KType -> Binding -> Bool
    inBinding t (Binding _ ts) = Set.member t ts

--------------------------------
---------- Statements ----------
--------------------------------

inferReturns :: MonadTC m => AST 'Stmt TypeAnnotated -> m (Set KType)
inferReturns (ExprStmt expr ) = types <$> infer expr
inferReturns (Block    stmts) = withState id $ do
    statementTypes <- mapM inferReturns stmts
    return $ lastDef (Set.singleton KUnit) statementTypes
inferReturns (While (WhileStmt cond blk)) = do
    check KBool cond
    _doesntMatter <- inferReturns blk
    return (Set.singleton KUnit)
inferReturns (If (IfStmt cond thenBlk elseBlk)) = do
    check KBool cond
    thenBlkType <- inferReturns thenBlk
    elseBlkType <- inferReturns elseBlk
    assert
        (not $ null (thenBlkType `Set.intersection` elseBlkType))
        (  "True branch returns "
        <> show thenBlkType
        <> " false branch returns "
        <> show elseBlkType
        )
    return (thenBlkType `Set.intersection` elseBlkType)
inferReturns (Assign accessor expr) = do
    Binding _              inferedTypes <- infer expr
    Binding nameMutability nameTypes    <- inferAccessor accessor

    assert (nameMutability == Variable) (show accessor <> " is constant ")
    assert (not $ null (nameTypes `Set.intersection` inferedTypes))
           "Can't assign a value of "
    return (Set.singleton KUnit)
inferReturns (Var name declaredType expr) = do
    checkBinding name declaredType expr
    modify (addBinding (Identifier name) (Binding Variable [declaredType]))
    return (Set.singleton KUnit)
inferReturns (Let name declaredType expr) = do
    checkBinding name declaredType expr
    modify (addBinding (Identifier name) (Binding Constant [declaredType]))
    return (Set.singleton KUnit)

inferAccessor
    :: MonadTC m => WriteAccess (AnnotatedName 'NoAnnotation) -> m Binding
inferAccessor (WriteAccess name path) = do
    Binding mutability baseType <- lookupName (toIdentifier name)
    finalType                   <- foldlM folder baseType path
    return (Binding mutability finalType)
  where
    folder
        :: MonadTC m
        => Set KType
        -> AnnotatedName 'NoAnnotation
        -> m (Set KType)
    folder baseTypes (Name fieldName) = do
        let fieldTypes =
                catSetMaybes $ Set.map (findFieldTypes fieldName) baseTypes
        if null fieldTypes
            then throwError ("Can't find field " <> fieldName)
            else return fieldTypes

    findFieldTypes :: Name -> KType -> Maybe KType
    findFieldTypes fieldName (KUserType _ fieldMap) = lookup fieldName fieldMap
    findFieldTypes _         _                      = Nothing

    catSetMaybes :: Ord a => Set (Maybe a) -> Set a
    catSetMaybes = foldl appendMaybe Set.empty
      where
        appendMaybe :: Ord a => Set a -> Maybe a -> Set a
        appendMaybe s Nothing  = s
        appendMaybe s (Just e) = Set.insert e s

checkReturns :: MonadTC m => KType -> AST 'Stmt TypeAnnotated -> m ()
checkReturns t (ExprStmt expr) = check t expr
checkReturns t blk@Block{}     = do
    returnTypes <- inferReturns blk
    assert (Set.member t returnTypes)
           ("The return type is not one of" <> show returnTypes)
checkReturns t stmt@While{} = do
    _ <- inferReturns stmt
    assert (t == KUnit) "While block always returns Unit"
checkReturns t stmt@If{} = do
    returnTypes <- inferReturns stmt
    assert
        (Set.member t returnTypes)
        ("If block returns one of" <> show returnTypes <> ", not " <> show t)
checkReturns t stmt@Assign{} = do
    _ <- inferReturns stmt
    assert (t == KUnit) "Assignment always returns Unit"
checkReturns t stmt@Var{} = do
    _ <- inferReturns stmt
    assert (t == KUnit) "Var always returns Unit"
checkReturns t stmt@Let{} = do
    _ <- inferReturns stmt
    assert (t == KUnit) "Let always returns Unit"

checkBinding :: MonadTC m => Name -> KType -> AST 'Expr TypeAnnotated -> m ()
checkBinding name declaredType expr = do
    existingBinding <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding)
           ("Binding for " <> show name <> "already exists")
    check declaredType expr

-----------------------------
---------- Helpers ----------
-----------------------------

lookupName :: MonadTC m => Identifier 'NoAnnotation -> m Binding
lookupName name = gets (Map.lookup name . bindings) >>= \case
    Just binding -> pure binding
    Nothing      -> throwError (show name <> " is not in context")

assert :: MonadError e m => Bool -> e -> m ()
assert True  _   = pure ()
assert False err = throwError err

-- | Add a set of function arguments to a TypeCtx.
addArgs :: [(Name, KType)] -> TypeCtx -> TypeCtx
addArgs args ctx =
    let bindings = (Binding Constant . Set.singleton) . snd <$> args
    in  let names = Identifier . fst <$> args
        in  ctx <> TypeCtx Map.empty (Map.fromList (zip names bindings))
