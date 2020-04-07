{-# LANGUAGE OverloadedLists #-}
module Kima.Types.Bidirectional
    ( MonadTC
    , infer
    , check
    , inferReturns
    , checkReturns
    , checkProgram
    , checkTopLevel
    , runTypeChecking
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

import           Control.Monad.Except
import           Control.Monad.State.Extended

import           Data.Bifunctor
import           Data.Functor
import           Data.Maybe
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Kima.AST
import           Kima.Types.Errors
import           Kima.Types.TypeCtx

type MonadTC m = (MonadState TypeCtx m, MonadError TypecheckingError m)

runTypeChecking ctx action = evalStateT action ctx

---------------------------------
---------- Type subsumption -----
---------------------------------

isSubEffect :: KEffect -> KEffect -> Bool
isSubEffect (KEffect _ subOps) (KEffect _ superOps) =
    and [ op `elem` superOps | op <- subOps ]


-- | Check whether a type 'fits into' another. Currently checks for
-- * Subeffects
-- * Subsumption in function arguments/return types (contravariantly)
--
-- If/when parametricity is added it will be added here.
-- We could also possibly move overloading checking to here
subsumedBy :: KType -> KType -> Bool
subsumedBy t1 t2 | t1 == t2 = True
subsumedBy (KFunc argList1 eff1 rt1) (KFunc argList2 eff2 rt2) = and @[]
    [ length argList1 == length argList2
    , and (zipWith subsumedBy argList2 argList1)
    , rt1 `subsumedBy` rt2
    , eff1 `isSubEffect` eff2
    ]
subsumedBy _ _ = False

---------------------------------
---------- Expressions ----------
---------------------------------

-- | Try to infer the type of an expression. Returns the typed expression as
-- well as its type. Throws an error if a type can't be assigned to the
-- expression *or* if there are multiple possible types
infer :: forall m. MonadTC m => Expr TypeAnnotated -> m (Expr Typed, KType)
infer (LiteralExpr    lit@(IntExpr    _)) = pure (LiteralExpr lit, KInt)
infer (LiteralExpr    lit@(FloatExpr  _)) = pure (LiteralExpr lit, KFloat)
infer (LiteralExpr    lit@(BoolExpr   _)) = pure (LiteralExpr lit, KBool)
infer (LiteralExpr    lit@(StringExpr _)) = pure (LiteralExpr lit, KString)
infer (IdentifierExpr name) = (lookupName name <&> types) <&> Set.toList >>= \case
    -- There needs to be only a single possibility for the type. Otherwise we
    -- have an ambiguity
    -- Also. No need to check for empty case since that would have thrown an
    -- error in lookupName
    [t]   -> pure (IdentifierExpr (typeAnnotate t name), t)
    types -> throwError (AmbiguousName name types)
infer (FuncExpr (ensureTypedArgs -> Just args) eff maybeRt body) = do
    let KEffect _ ops = eff
    (typedBody, rt) <- case maybeRt of
        Just rt -> (, rt) <$> withState
            (setActiveOperations ops . addArgs args)
            (checkReturns rt body)
        Nothing -> withState (addArgs args) (inferReturns body)

    let functionType  = KFunc (snd <$> args) eff rt
    let typedFuncExpr = FuncExpr args eff rt typedBody
    return (typedFuncExpr, functionType)
infer FuncExpr{} = throwError MissingArgumentTypes
infer (Call callee args) = do
    calleeTypes     <- Set.toList <$> enumerateTypes callee
    availableEffect <- gets activeEffect

    possibleResults <- forM calleeTypes $ \case
        calleeType@(KFunc argTypes calleeEff returnType) ->
            do
                    typedArgs   <- checkArgList argTypes args
                    typedCallee <- check calleeType callee
                    return
                        $ Just
                              ( calleeEff
                              , (Call typedCallee typedArgs, returnType)
                              )
                `catchError` const (pure Nothing)
        _ -> pure Nothing

    case catMaybes possibleResults of
        [(calleeEffect, result)] -> do
            assert (calleeEffect `isSubEffect` availableEffect)
                   (UnavailableEffect availableEffect calleeEffect)
            return result
        results@(_ : _) -> throwError (AmbiguousCall (snd . snd <$> results))
        []              -> throwError (NoMatchingFunction calleeTypes)
infer (Handle expr handlers) = do
    (typedHandlers, availableOps) <- unzip <$> traverse inferHandler handlers
    (typedExpr, exprType) <- withState (addActiveOperations availableOps) $ infer expr
    return (Handle typedExpr typedHandlers, exprType)

inferHandler :: MonadTC m => HandlerClause TypeAnnotated -> m (HandlerClause Typed, KOperation)
inferHandler (HandlerClause name (ensureTypedArgs -> Just args) (Just rt) body) = do
    let inferedOp = KOperation name (snd <$> args) rt
    allOperations <- gets operations
    assert (inferedOp `elem` allOperations) (NonExistentOperation inferedOp)

    typedBody <- withState (addArgs args) $ checkReturns rt body
    let typedHandler = HandlerClause name args rt typedBody

    pure (typedHandler, inferedOp)
inferHandler HandlerClause{ returnType = Just _ } = throwError MissingArgumentTypes
inferHandler HandlerClause{ returnType = Nothing } = throwError MissingReturnType

-- | List all possible types for an expression
enumerateTypes :: MonadTC m => Expr TypeAnnotated -> m (Set KType)
enumerateTypes (LiteralExpr    IntExpr{}   ) = pure [KInt]
enumerateTypes (LiteralExpr    FloatExpr{} ) = pure [KFloat]
enumerateTypes (LiteralExpr    BoolExpr{}  ) = pure [KBool]
enumerateTypes (LiteralExpr    StringExpr{}) = pure [KString]
enumerateTypes (IdentifierExpr ident       ) = types <$> lookupName ident
enumerateTypes (FuncExpr (fmap (fmap snd) . ensureTypedArgs -> Just argTypes) eff (Just rt) _)
    = pure [KFunc argTypes eff rt]
enumerateTypes FuncExpr{}         = throwError MissingArgumentTypes
-- TODO: When enumerating the types of a handler expr, take the handlers into account
enumerateTypes (Handle expr _) = enumerateTypes expr
enumerateTypes (Call callee args) = do
    calleeTypes <- Set.toList <$> enumerateTypes callee
    argTypeSets <- fmap Set.toList <$> mapM enumerateTypes args

    -- Possible sequences of arg types
    let argTypeSequences = cartesianProduct argTypeSets
    -- Possible combinations of call types and arg types
    let callCombinations =
            [ (c, as) | c <- calleeTypes, as <- argTypeSequences ]

    let returnTypes = catMaybes $ uncurry returnsWithArgs <$> callCombinations
    return (Set.fromList returnTypes)
  where
    returnsWithArgs :: KType -> [KType] -> Maybe KType
    returnsWithArgs (KFunc argTypes' _eff rt) argTypes | argTypes == argTypes' =
        Just rt
    returnsWithArgs _ _ = Nothing

-- | Check that an expression has a certain type. If it applies, return the
-- expression with the type applied. If not, throw an appropriate error.
check :: MonadTC m => KType -> Expr TypeAnnotated -> m (Expr Typed)
check expectedType (IdentifierExpr ident) = do
    Binding _ availableTypes <- lookupName ident

    let possibleTypes = Set.filter (`subsumedBy` expectedType) availableTypes

    case Set.toList possibleTypes of
        [        result] -> return (IdentifierExpr (typeAnnotate result ident))
        results@(_ : _ ) -> throwError (AmbiguousCall results)
        []               -> throwError
            (UnavailableType (Set.toList availableTypes) expectedType)
check expectedType (Call callee args) = do
    (typedArgs, argTypes) <- unzip <$> mapM infer args
    callEffect            <- gets activeEffect

    typedCallee <- check (KFunc argTypes callEffect expectedType) callee

    return (Call typedCallee typedArgs)
check expectedType expr = do
    (typedExpr, inferedType) <- infer expr
    assert (inferedType `subsumedBy` expectedType)
           (UnexpectedType expectedType inferedType)
    return typedExpr

--------------------------------
---------- Statements ----------
--------------------------------

-- | Try to infer the return type of a statement. If it can be typed, return the
-- typed statement and the inferred return type. If not, throw an appropriate
-- error
inferReturns :: MonadTC m => Stmt TypeAnnotated -> m (Stmt Typed, KType)
inferReturns (ExprStmt expr ) = first ExprStmt <$> infer expr
inferReturns (Block    stmts) = withState id $ do
    (typedStatements, statementReturnTypes) <- unzip <$> mapM inferReturns stmts
    return (Block typedStatements, lastDef KUnit statementReturnTypes)
inferReturns (While (WhileStmt cond blk)) = do
    typedCond     <- check KBool cond
    (typedBlk, _) <- inferReturns blk

    let typedWhile = While (WhileStmt typedCond typedBlk)
    return (typedWhile, KUnit)
inferReturns (If (IfStmt cond thenBlk elseBlk)) = do
    typedCond                   <- check KBool cond
    (typedThenBlk, thenBlkType) <- inferReturns thenBlk
    (typedElseBlk, elseBlkType) <- inferReturns elseBlk
    assert (thenBlkType == elseBlkType) (MismatchedIf thenBlkType elseBlkType)

    let typedIf = If (IfStmt typedCond typedThenBlk typedElseBlk)
    return (typedIf, thenBlkType)
inferReturns (Assign accessor expr) = do
    (typedExpr    , inferedType                     ) <- infer expr
    (typedAccessor, Binding nameMutability nameTypes) <- inferAccessor accessor

    assert (nameMutability == Variable) (AssignToConst accessor)
    assert (inferedType `Set.member` nameTypes)
           (UnavailableType (Set.toList nameTypes) inferedType)

    let typedAssign = Assign typedAccessor typedExpr
    return (typedAssign, KUnit)
inferReturns (Var name (Just declaredType) expr) = do
    typedExpr       <- check declaredType expr

    existingBinding <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Variable [declaredType]))

    let typedVar = Var name declaredType typedExpr
    return (typedVar, KUnit)
inferReturns (Var name Nothing expr) = do
    (typedExpr, exprType) <- infer expr

    existingBinding       <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Variable [exprType]))

    let typedVar = Var name exprType typedExpr
    return (typedVar, KUnit)
inferReturns (Let name (Just declaredType) expr) = do
    typedExpr       <- check declaredType expr

    existingBinding <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Constant [declaredType]))

    let typedLet = Let name declaredType typedExpr
    return (typedLet, KUnit)
inferReturns (Let name Nothing expr) = do
    (typedExpr, exprType) <- infer expr

    existingBinding       <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Constant [exprType]))

    let typedLet = Let name exprType typedExpr
    return (typedLet, KUnit)

-- | Try to infer the binding an accessor refers to.
inferAccessor
    :: MonadTC m
    => WriteAccess (AnnotatedName 'NoAnnotation)
    -> m (WriteAccess (AnnotatedName ( 'Annotation KType)), Binding)
inferAccessor (WriteAccess name path) = do
    nameBinding <- lookupName (toIdentifier name)
    case nameBinding of
        Binding mutability [baseType] -> do
            (finalType, typedPath) <- foldPath baseType path
            return
                ( WriteAccess (typeAnnotate baseType name) typedPath
                , Binding mutability [finalType]
                )
        Binding _ types ->
            throwError (AmbiguousName (toIdentifier name) (Set.toList types))
  where
    foldPath
        :: MonadTC m
        => KType
        -> [AnnotatedName 'NoAnnotation]
        -> m (KType, [AnnotatedName ( 'Annotation KType)])
    foldPath baseType@(KUserType _ baseTypeFields) (Name thisField : restPath)
        = case lookup thisField baseTypeFields of
            Just fieldType -> do
                (finalType, restTypedPath) <- foldPath fieldType restPath
                return (finalType, TName thisField fieldType : restTypedPath)
            Nothing -> throwError (NoSuchField baseType thisField)
    foldPath t []                   = pure (t, [])
    foldPath t (Name thisField : _) = throwError (NoSuchField t thisField)

-- | Check that a statement returns a given type. If it does, return the typed
-- statement, otherwise, throw an appropriate error
checkReturns :: MonadTC m => KType -> Stmt TypeAnnotated -> m (Stmt Typed)
checkReturns KUnit (ExprStmt expr) =
    -- If it's not Unit then anything else will do, doesn't matter
    ExprStmt <$> (check KUnit expr `catchError` const (fst <$> infer expr))
checkReturns t            (ExprStmt expr) = ExprStmt <$> check t expr
checkReturns expectedType stmt            = do
    (typedBlk, returnType) <- inferReturns stmt

    -- When we're expecting Unit, anything else will do
    when (expectedType /= KUnit) $ assert
        (returnType `subsumedBy` expectedType)
        (UnexpectedType expectedType returnType)
    return typedBlk

-----------------------------
--------- Top-level AST -----
-----------------------------

-- | Try to typecheck a module
checkProgram :: MonadTC m => Module TypeAnnotated -> m (Module Typed)
checkProgram (Program decls) = Program <$> mapM checkTopLevel decls

-- | Try to typecheck a top-level declaration
checkTopLevel :: MonadTC m => TopLevel TypeAnnotated -> m (TopLevel Typed)
checkTopLevel (FuncDef name (ensureTypedArgs -> Just args) eff (Just rt) body)
    =   FuncDef name args eff rt
    <$> withState (setEffect eff . addArgs args) (checkReturns rt body)
checkTopLevel (FuncDef name (ensureTypedArgs -> Just args) eff Nothing body) =
    do
        (typedBody, rt) <- withState (setEffect eff . addArgs args)
                                     (inferReturns body)
        return (FuncDef name args eff rt typedBody)
checkTopLevel FuncDef{} = throwError MissingArgumentTypes
checkTopLevel (DataDef name (ensureTypedArgs -> Just typeFields)) =
    pure (DataDef name typeFields)
checkTopLevel DataDef{} = throwError MissingFieldTypes
checkTopLevel (OperationDef name (ensureTypedArgs -> Just args) (Just rt)) =
    pure (OperationDef name args rt)
checkTopLevel (OperationDef _ (ensureTypedArgs -> Just _) Nothing) = throwError MissingReturnType
checkTopLevel OperationDef{} = throwError MissingArgumentTypes
checkTopLevel (EffectSynonymDef name ops) = pure (EffectSynonymDef name ops)
-----------------------------
---------- Helpers ----------
-----------------------------

-- | Look an identifier up in the current context. If it's not present, throw an
-- appropriate error
lookupName :: MonadTC m => Identifier 'NoAnnotation -> m Binding
lookupName name = gets (Map.lookup name . bindings) >>= \case
    Just binding -> pure binding
    Nothing      -> throwError (UnboundName name)

assert :: MonadError e m => Bool -> e -> m ()
assert True  _   = pure ()
assert False err = throwError err

ensureTypedArgs :: [(a, Maybe b)] -> Maybe [(a, b)]
ensureTypedArgs = traverse sequence

-- | Add a set of function arguments to a TypeCtx.
addArgs :: [(Name, KType)] -> TypeCtx -> TypeCtx
addArgs args ctx =
    ctx <> (mempty { bindings = Map.fromList (zip names bindings) }) :: TypeCtx
  where
    bindings = (Binding Constant . Set.singleton) . snd <$> args
    names    = Identifier . fst <$> args

setEffect :: KEffect -> TypeCtx -> TypeCtx
setEffect eff ctx = ctx { activeEffect = eff }

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []         = [[]]
cartesianProduct (xs : xss) = [ x : ys | x <- xs, ys <- yss ]
    where yss = cartesianProduct xss

checkArgList :: MonadTC m => [KType] -> [Expr TypeAnnotated] -> m [Expr Typed]
checkArgList argTypes args = do
    assert (length argTypes == length args) (WrongArgumentCount (length argTypes) (length args))
    zipWithM check argTypes args
        `catchError` const (throwError (WrongArgumentTypes argTypes))
