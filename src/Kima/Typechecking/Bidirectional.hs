{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.Bidirectional
    ( MonadTC
    , runTypeChecking
    , checkProgram
    , checkTopLevel
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

import           Control.Applicative
import           Control.Monad.State.Extended
import           Control.Monad.Except

import           Data.Functor
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Kima.AST
import           Kima.KimaTypes
import           Kima.Typechecking.TypeCtx

type MonadTC m = (MonadState TypeCtx m, MonadError String m, Alternative m)

runTypeChecking ctx action = runExcept (evalStateT action ctx)

---------------------------------
---------- Expressions ----------
---------------------------------

infer
    :: MonadTC m => AST 'Expr TypeAnnotated -> m (AST 'Expr Typed, Set KType)
infer expr@(LiteralE lit@(IntExpr _)) =
    (LiteralE lit, ) <$> enumerateTypes expr
infer expr@(LiteralE lit@(FloatExpr _)) =
    (LiteralE lit, ) <$> enumerateTypes expr
infer expr@(LiteralE lit@(BoolExpr _)) =
    (LiteralE lit, ) <$> enumerateTypes expr
infer expr@(LiteralE lit@(StringExpr _)) =
    (LiteralE lit, ) <$> enumerateTypes expr
infer expr@(IdentifierE name) = enumerateTypes expr <&> Set.toList >>= \case
    -- | There needs to be only a single possibility for the type. Otherwise we
    -- | have an ambiguity
    [t]   -> pure (IdentifierE (typeAnnotate t name), [t])
    types -> throwError
        (  "Ambiguous type for "
        <> show name
        <> ". Possible types are: "
        <> show types
        )
infer (FuncExpr args rt body) = do
    let expectedType = KFunc ((snd <$> args) $-> rt)
    typedBody <- withState (addArgs args) $ checkReturns expectedType body

    let typedFuncExpr = FuncExpr args rt typedBody
    return (typedFuncExpr, [expectedType])
infer (Call callee args) = do
    calleeTypes     <- Set.toList <$> enumerateTypes callee

    possibleResults <- forM calleeTypes $ \case
        calleeType@(KFunc (Signature argTypes returnType)) ->
            do
                    typedArgs   <- zipWithM check argTypes args
                    typedCallee <- check calleeType callee
                    return $ Just (Call typedCallee typedArgs, [returnType])
                `catchError` const (pure Nothing)
        _ -> pure Nothing

    case catMaybes possibleResults of
        [ result] -> return result
        _ :     _ -> throwError "Ambiguous call"
        []        -> throwError "No matching call"

enumerateTypes :: MonadTC m => AST 'Expr TypeAnnotated -> m (Set KType)
enumerateTypes (LiteralE    IntExpr{}   ) = pure [KInt]
enumerateTypes (LiteralE    FloatExpr{} ) = pure [KFloat]
enumerateTypes (LiteralE    BoolExpr{}  ) = pure [KBool]
enumerateTypes (LiteralE    StringExpr{}) = pure [KString]
enumerateTypes (IdentifierE ident       ) = types <$> lookupName ident
enumerateTypes (FuncExpr (fmap snd -> argTypes) rt _) =
    pure [KFunc (argTypes $-> rt)]
enumerateTypes (Call callee args) = do
    calleeTypes <- Set.toList <$> enumerateTypes callee
    -- Possible types for each arg
    argTypes    <- fmap Set.toList <$> mapM enumerateTypes args
    -- Possible sequences of arg types
    let argSequences = cartprod argTypes
    let returnTypes =
            catMaybes $ zipWith returnsWithArgs argSequences calleeTypes

    return (Set.fromList returnTypes)
  where
    returnsWithArgs :: [KType] -> KType -> Maybe KType
    returnsWithArgs argTypes (KFunc (Signature argTypes' rt))
        | argTypes == argTypes' = Just rt
    returnsWithArgs _ _ = Nothing

check :: MonadTC m => KType -> AST 'Expr TypeAnnotated -> m (AST 'Expr Typed)
check expectedType (IdentifierE ident) = lookupName ident >>= \case
    (Binding _ availableTypes)
        | expectedType `Set.member` availableTypes -> pure
        $  IdentifierE (typeAnnotate expectedType ident)
        | otherwise -> throwError
            (  "Identifier "
            <> show ident
            <> " is not available with type "
            <> show expectedType
            )
check expectedType (Call callee args) = do
    (typedArgs, argTypes) <- unzip <$> mapM infer args

    let possibleArgSets = cartprod (Set.toList <$> argTypes)
    possibleTypedCallees :: [AST 'Expr Typed] <- catMaybes <$> forM
        possibleArgSets
        (\theseArgTypes ->
            (Just <$> check (KFunc (theseArgTypes $-> expectedType)) callee)
                `catchError` const (pure Nothing)
        )

    case possibleTypedCallees of
        [ typedCallee] -> return (Call typedCallee typedArgs)
        _ :          _ -> throwError "Ambiguous call"
        []             -> throwError "No matching call"

check expectedType expr = do
    (typedExpr, inferedTypes) <- infer expr
    assert (expectedType `Set.member` inferedTypes)
           (show expectedType <> " is not one of " <> show inferedTypes)
    return typedExpr

--------------------------------
---------- Statements ----------
--------------------------------

inferReturns
    :: MonadTC m => AST 'Stmt TypeAnnotated -> m (AST 'Stmt Typed, Set KType)
inferReturns (ExprStmt expr ) = first ExprStmt <$> infer expr
inferReturns (Block    stmts) = withState id $ do
    (typedStatements, statementReturnTypes) <- unzip <$> mapM inferReturns stmts
    return (Block typedStatements, lastDef [KUnit] statementReturnTypes)
   -- return $ lastDef (Set.singleton KUnit) statementTypes
inferReturns (While (WhileStmt cond blk)) = do
    typedCond     <- check KBool cond
    (typedBlk, _) <- inferReturns blk

    let typedWhile = While (WhileStmt typedCond typedBlk)
    return (typedWhile, [KUnit])
inferReturns (If (IfStmt cond thenBlk elseBlk)) = do
    typedCond                   <- check KBool cond
    (typedThenBlk, thenBlkType) <- inferReturns thenBlk
    (typedElseBlk, elseBlkType) <- inferReturns elseBlk
    assert
        (not $ null (thenBlkType `Set.intersection` elseBlkType))
        (  "True branch returns "
        <> show thenBlkType
        <> " false branch returns "
        <> show elseBlkType
        )

    let typedIf = If (IfStmt typedCond typedThenBlk typedElseBlk)
    return (typedIf, thenBlkType `Set.intersection` elseBlkType)
inferReturns (Assign accessor expr) = do
    (typedExpr    , inferedTypes                    ) <- infer expr
    (typedAccessor, Binding nameMutability nameTypes) <- inferAccessor accessor

    assert (nameMutability == Variable) (show accessor <> " is constant ")
    assert (not $ null (nameTypes `Set.intersection` inferedTypes))
           "Can't assign a value of "

    let typedAssign = Assign typedAccessor typedExpr
    return (typedAssign, [KUnit])
inferReturns (Var name declaredType expr) = do
    typedExpr       <- check declaredType expr

    existingBinding <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding)
           ("Binding for " <> show name <> "already exists")
    modify (addBinding (Identifier name) (Binding Variable [declaredType]))

    let typedVar = Var name declaredType typedExpr
    return (typedVar, [KUnit])
inferReturns (Let name declaredType expr) = do
    typedExpr       <- check declaredType expr

    existingBinding <- gets (Map.lookup (Identifier name) . bindings)
    assert (isNothing existingBinding)
           ("Binding for " <> show name <> "already exists")
    modify (addBinding (Identifier name) (Binding Constant [declaredType]))

    let typedLet = Let name declaredType typedExpr
    return (typedLet, [KUnit])

inferAccessor
    :: MonadTC m
    => WriteAccess (AnnotatedName 'NoAnnotation)
    -> m (WriteAccess (AnnotatedName ( 'Annotation KType)), Binding)
inferAccessor (WriteAccess name path) =
    lookupName (toIdentifier name) >>= \case
        Binding mutability [baseType] -> do
            (finalType, typedPath) <- foldPath baseType path
            return
                ( WriteAccess (typeAnnotate baseType name) typedPath
                , Binding mutability [finalType]
                )
        Binding _ _ -> throwError ("Ambiguous type for " <> show name)
  where
    foldPath
        :: MonadTC m
        => KType
        -> [AnnotatedName 'NoAnnotation]
        -> m (KType, [AnnotatedName ( 'Annotation KType)])
    foldPath (KUserType typeName typeFields) (Name thisField : restPath) =
        case lookup thisField typeFields of
            Just fieldType -> do
                (finalType, restTypedPath) <- foldPath fieldType restPath
                return (finalType, TName thisField fieldType : restTypedPath)
            Nothing -> throwError
                ("Type " <> typeName <> " has no field " <> thisField)
    foldPath t [] = pure (t, [])
    foldPath t (Name thisField : _) =
        throwError ("Type " <> show t <> " has no field " <> thisField)

checkReturns
    :: MonadTC m => KType -> AST 'Stmt TypeAnnotated -> m (AST 'Stmt Typed)
checkReturns t (ExprStmt expr) = ExprStmt <$> check t expr
checkReturns t blk@Block{}     = do
    (typedBlk, returnTypes) <- inferReturns blk
    assert (Set.member t returnTypes)
           ("The return type is not one of" <> show returnTypes)
    return typedBlk
checkReturns t stmt@While{} = do
    (typedWhile, _) <- inferReturns stmt
    assert (t == KUnit) "While block always returns Unit"
    return typedWhile
checkReturns t stmt@If{} = do
    (typedIf, returnTypes) <- inferReturns stmt
    assert
        (Set.member t returnTypes)
        ("If block returns one of" <> show returnTypes <> ", not " <> show t)
    return typedIf
checkReturns t stmt@Assign{} = do
    (typedAssign, _) <- inferReturns stmt
    assert (t == KUnit) "Assignment always returns Unit"
    return typedAssign
checkReturns t stmt@Var{} = do
    (typedVar, _) <- inferReturns stmt
    assert (t == KUnit) "Var always returns Unit"
    return typedVar
checkReturns t stmt@Let{} = do
    (typedLet, _) <- inferReturns stmt
    assert (t == KUnit) "Let always returns Unit"
    return typedLet

-----------------------------
--------- Top-level AST -----
-----------------------------

checkProgram
    :: MonadTC m => AST 'Module TypeAnnotated -> m (AST 'Module Typed)
checkProgram (Program decls) = Program <$> mapM checkTopLevel decls

checkTopLevel
    :: MonadTC m => AST 'TopLevel TypeAnnotated -> m (AST 'TopLevel Typed)
checkTopLevel (FuncDef name args rt body) =
    FuncDef name args rt <$> withState (addArgs args) (checkReturns rt body)
checkTopLevel (DataDef name typeFields) = pure (DataDef name typeFields)

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

cartprod :: [[a]] -> [[a]]
cartprod []         = [[]]
cartprod (xs : xss) = [ x : ys | x <- xs, ys <- yss ] where yss = cartprod xss
