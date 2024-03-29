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
infer (LiteralExpr    lit@(IntLit    _)) = pure (LiteralExpr lit, KInt)
infer (LiteralExpr    lit@(FloatLit  _)) = pure (LiteralExpr lit, KFloat)
infer (LiteralExpr    lit@(BoolLit   _)) = pure (LiteralExpr lit, KBool)
infer (LiteralExpr    lit@(StringLit _)) = pure (LiteralExpr lit, KString)
infer (LiteralExpr    lit@UnitLit      ) = pure (LiteralExpr lit, KUnit)
infer (IdentifierExpr name) = (lookupName name <&> types) <&> Set.toList >>= \case
    -- There needs to be only a single possibility for the type. Otherwise we
    -- have an ambiguity
    -- Also. No need to check for empty case since that would have thrown an
    -- error in lookupName
    [t]   -> pure (IdentifierExpr (typeAnnotate t name), t)
    types -> throwError (AmbiguousName name types)
infer (FuncExpr (TypedArgs args) eff maybeRt body) = do
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
infer (CallExpr callee args) = do
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
                              , (CallExpr typedCallee typedArgs, returnType)
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
infer (HandleExpr body handlers) = do
    -- We need to infer the type of the expression before we check the handlers.
    -- This is so that we know what break statements should return.
    availableOps          <- traverse getOp handlers
    (typedExpr, exprType) <- withState (addActiveOperations availableOps)
        $ inferReturns body
    typedHandlers <- withState (addActiveOperations availableOps . setHandlerResult exprType)
        $ zipWithM checkHandler availableOps handlers
    return (HandleExpr typedExpr typedHandlers, exprType)
  where
    getOp (HandlerClause name (TypedArgs args) (Just rt) _) = do
        let inferedOp = KOperation name (snd <$> args) rt
        allOperations <- gets operations
        assert (inferedOp `elem` allOperations) (NonExistentOperation inferedOp)
        return inferedOp
    getOp HandlerClause { returnType = Just _ } =
        throwError MissingArgumentTypes
    getOp HandlerClause { returnType = Nothing } = throwError MissingReturnType

    checkHandler (KOperation name argTypes rt) (HandlerClause _ (map fst -> argNames) _ handlerBody)
        = withState (addArgs (zip argNames argTypes))
            $   HandlerClause name (zip argNames argTypes) rt
            <$> checkReturns rt handlerBody
infer (MatchExpr expr clauses) = do
    assert (not $ null clauses) EmptyClauses

    (typedExpr, exprType) <- infer expr

    (unzip -> (typedClauses, clauseTypes)) <- forM clauses $ \(MatchClause pat stmt) -> do
        (typedPattern, addedCtx) <- checkPattern exprType pat
        (typedStmt, rt) <- withState (addArgs addedCtx) $ inferReturns stmt
        pure (MatchClause typedPattern typedStmt, rt)

    assert (allEq clauseTypes) (MismatchedClauseTypes clauseTypes)

    return (MatchExpr typedExpr typedClauses, head clauseTypes)

checkPattern :: MonadTC m => KType -> Pattern TypeAnnotated -> m (Pattern Typed, [(Name, KType)])
checkPattern t (WildcardPattern n (Just t')) =
    if t == t'
    then pure (WildcardPattern n t, [(n, t)])
    else throwError (UnexpectedType t t')
checkPattern t (WildcardPattern n Nothing) = pure (WildcardPattern n t, [(n, t)])
checkPattern t (ConstructorPattern n argPats) =
    gets (Map.lookup (t, n) . constructorBindings) >>= \case
        Just argTypes -> do
            (typedArgPats, argPatCtxs) <- unzip <$> zipWithM checkPattern argTypes argPats
            return (ConstructorPattern n typedArgPats, concat argPatCtxs)
        Nothing -> throwError (NonExistentConstructor t n)

-- | List all possible types for an expression
enumerateTypes :: MonadTC m => Expr TypeAnnotated -> m (Set KType)
enumerateTypes (LiteralExpr    IntLit{}   ) = pure [KInt]
enumerateTypes (LiteralExpr    FloatLit{} ) = pure [KFloat]
enumerateTypes (LiteralExpr    BoolLit{}  ) = pure [KBool]
enumerateTypes (LiteralExpr    StringLit{}) = pure [KString]
enumerateTypes (LiteralExpr    UnitLit    ) = pure [KUnit]
enumerateTypes (IdentifierExpr ident       ) = types <$> lookupName ident
enumerateTypes (FuncExpr (TypedArgs (fmap snd -> argTypes)) eff (Just rt) _)
    = pure [KFunc argTypes eff rt]
enumerateTypes FuncExpr{}         = throwError MissingArgumentTypes
-- TODO: Find a better way to handle this
enumerateTypes HandleExpr{} = throwError AmbiguousHandler
enumerateTypes MatchExpr{} = throwError AmbiguousMatch
enumerateTypes (CallExpr callee args) = do
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
check expectedType (CallExpr callee args) = do
    (typedArgs, argTypes) <- unzip <$> mapM infer args
    callEffect            <- gets activeEffect

    typedCallee <- check (KFunc argTypes callEffect expectedType) callee

    return (CallExpr typedCallee typedArgs)
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
inferReturns (BlockStmt    stmts) = withState id $ do
    (typedStatements, statementReturnTypes) <- unzip <$> mapM inferReturns stmts
    return (BlockStmt typedStatements, lastDef KUnit statementReturnTypes)
inferReturns (WhileStmt (While cond blk)) = do
    typedCond     <- check KBool cond
    (typedBlk, _) <- inferReturns blk

    let typedWhile = WhileStmt (While typedCond typedBlk)
    return (typedWhile, KUnit)
inferReturns (IfStmt (If cond thenBlk elseBlk)) = do
    typedCond                   <- check KBool cond
    (typedThenBlk, thenBlkType) <- inferReturns thenBlk
    (typedElseBlk, elseBlkType) <- inferReturns elseBlk
    assert (thenBlkType == elseBlkType) (MismatchedIf thenBlkType elseBlkType)

    let typedIf = IfStmt (If typedCond typedThenBlk typedElseBlk)
    return (typedIf, thenBlkType)
inferReturns (AssignStmt accessor expr) = do
    (typedExpr    , inferedType                     ) <- infer expr
    (typedAccessor, Binding nameMutability nameTypes) <- inferAccessor accessor

    assert (nameMutability == Variable) (AssignToConst accessor)
    assert (inferedType `Set.member` nameTypes)
           (UnavailableType (Set.toList nameTypes) inferedType)

    let typedAssign = AssignStmt typedAccessor typedExpr
    return (typedAssign, KUnit)
inferReturns (VarStmt name (Just declaredType) expr) = do
    typedExpr       <- check declaredType expr

    existingBinding <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Variable [declaredType]))

    let typedVar = VarStmt name declaredType typedExpr
    return (typedVar, KUnit)
inferReturns (VarStmt name Nothing expr) = do
    (typedExpr, exprType) <- infer expr

    existingBinding       <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Variable [exprType]))

    let typedVar = VarStmt name exprType typedExpr
    return (typedVar, KUnit)
inferReturns (LetStmt name (Just declaredType) expr) = do
    typedExpr       <- check declaredType expr

    existingBinding <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Constant [declaredType]))

    let typedLet = LetStmt name declaredType typedExpr
    return (typedLet, KUnit)
inferReturns (LetStmt name Nothing expr) = do
    (typedExpr, exprType) <- infer expr

    existingBinding       <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding) (NameShadowed name)
    modify (addBinding (Identifier name) (Binding Constant [exprType]))

    let typedLet = LetStmt name exprType typedExpr
    return (typedLet, KUnit)
inferReturns (BreakStmt expr) = gets handlerResult >>= \case
    Just breakResultType -> do
        typedExpr <- check breakResultType expr
        return (BreakStmt typedExpr, KUnit)
    Nothing -> throwError UnexpectedBreak

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
    foldPath baseType (Name thisField : restPath) =
      gets (Map.lookup (baseType, thisField) . fieldBindings) >>= \case
        Just fieldType -> do
          (finalType, restTypedPath) <- foldPath fieldType restPath
          return (finalType, TName thisField fieldType : restTypedPath)
        Nothing -> throwError (NoSuchField baseType thisField)
    foldPath t [] = pure (t, [])

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
checkProgram (Module decls) = Module <$> mapM checkTopLevel decls

-- | Try to typecheck a top-level declaration
checkTopLevel :: MonadTC m => TopLevel TypeAnnotated -> m (TopLevel Typed)
checkTopLevel (FuncDef name (TypedArgs args) eff (Just rt) body)
    =   FuncDef name args eff rt
    <$> withState (setEffect eff . addArgs args) (checkReturns rt body)
checkTopLevel (FuncDef name (TypedArgs args) eff Nothing body) =
    do
        (typedBody, rt) <- withState (setEffect eff . addArgs args)
                                     (inferReturns body)
        return (FuncDef name args eff rt typedBody)
checkTopLevel FuncDef{} = throwError MissingArgumentTypes
checkTopLevel (ProductTypeDef name (TypedArgs typeFields)) =
    pure (ProductTypeDef name typeFields)
checkTopLevel ProductTypeDef{} = throwError MissingFieldTypes
checkTopLevel (SumTypeDef name (ensureTypedConstructors -> Just constructors)) =
    pure (SumTypeDef name constructors)
checkTopLevel SumTypeDef{} = throwError MissingFieldTypes
checkTopLevel (OperationDef name (TypedArgs args) (Just rt)) =
    pure (OperationDef name args rt)
checkTopLevel (OperationDef _ TypedArgs{} Nothing) = throwError MissingReturnType
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

pattern TypedArgs :: [(a, b)] -> [(a, Maybe b)]
pattern TypedArgs args <- (ensureTypedArgs -> Just args)

ensureTypedArgs :: [(a, Maybe b)] -> Maybe [(a, b)]
ensureTypedArgs = traverse sequence

ensureTypedConstructors :: [(name, [Maybe tExpr])] -> Maybe [(name, [tExpr])]
ensureTypedConstructors = traverse (traverse sequence)

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

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x : xs) = go xs
  where
    go [] = True
    go (x' : xs') = (x == x') && go xs'
