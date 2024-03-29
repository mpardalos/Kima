{-# LANGUAGE OverloadedLists #-}
module Kima.Types.TypeResolution
    ( resolveModuleTypes
    , resolveTopLevelTypes
    , resolveStmtTypes
    , resolveExprTypes
    )
where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bitraversable
import qualified Data.Map                      as Map
import           Kima.AST
import           Kima.Types.TypeCtx
import           Kima.Types.Errors

type MonadTypeResolution m
    = (MonadState TypeCtx m, MonadError TypecheckingError m)

-- | Resolve all typeExprs in an AST.
-- Note: in DataDefs, accessor types are annotated with the type of the attribute,
--       **not** their function type.
resolveModuleTypes
    :: MonadTypeResolution m => Module Desugared -> m (Module TypeAnnotated)
resolveModuleTypes (Module decls) = do
    processTopLevel decls
    Module <$> traverse resolveTopLevelTypes decls

resolveTopLevelTypes
    :: MonadTypeResolution m => TopLevel Desugared -> m (TopLevel TypeAnnotated)
resolveTopLevelTypes (FuncDef name argExprs effExpr rtExpr body) =
    FuncDef name
        <$> traverse (traverse (traverse resolveTypeExpr)) argExprs
        <*> resolveEffectExpr effExpr
        <*> traverse resolveTypeExpr rtExpr
        <*> resolveStmtTypes body
resolveTopLevelTypes (ProductTypeDef name args) =
    ProductTypeDef name <$> traverse (traverse (traverse resolveTypeExpr)) args
resolveTopLevelTypes (SumTypeDef name args) =
    SumTypeDef name <$> traverse (traverse (traverse (traverse resolveTypeExpr))) args
resolveTopLevelTypes (OperationDef name argExprs rtExpr ) =
    OperationDef name
        <$> traverse (traverse (traverse resolveTypeExpr)) argExprs
        <*> traverse resolveTypeExpr rtExpr
resolveTopLevelTypes (EffectSynonymDef name ops) = pure (EffectSynonymDef name ops)

resolveStmtTypes
    :: MonadTypeResolution m => Stmt Desugared -> m (Stmt TypeAnnotated)
resolveStmtTypes (ExprStmt expr ) = ExprStmt <$> resolveExprTypes expr
resolveStmtTypes (BlockStmt    stmts) = BlockStmt <$> traverse resolveStmtTypes stmts
resolveStmtTypes (WhileStmt stmt) =
    WhileStmt <$> bitraverse resolveExprTypes resolveStmtTypes stmt
resolveStmtTypes (IfStmt stmt) =
    IfStmt <$> bitraverse resolveExprTypes resolveStmtTypes stmt
resolveStmtTypes (AssignStmt access expr) = AssignStmt access <$> resolveExprTypes expr
resolveStmtTypes (VarStmt name t expr) =
    VarStmt name <$> traverse resolveTypeExpr t <*> resolveExprTypes expr
resolveStmtTypes (LetStmt name t expr) =
    LetStmt name <$> traverse resolveTypeExpr t <*> resolveExprTypes expr
resolveStmtTypes (BreakStmt expr) = BreakStmt <$> resolveExprTypes expr

resolveExprTypes
    :: MonadTypeResolution m => Expr Desugared -> m (Expr TypeAnnotated)
resolveExprTypes (FuncExpr argExprs effExpr rtExpr body) =
    FuncExpr
        <$> traverse (traverse (traverse resolveTypeExpr)) argExprs
        <*> resolveEffectExpr effExpr
        <*> traverse resolveTypeExpr rtExpr
        <*> resolveStmtTypes body
resolveExprTypes (CallExpr callee args) =
    CallExpr <$> resolveExprTypes callee <*> traverse resolveExprTypes args
resolveExprTypes (LiteralExpr    lit ) = pure (LiteralExpr lit)
resolveExprTypes (IdentifierExpr name) = pure (IdentifierExpr name)
resolveExprTypes (HandleExpr body handlers) =
    HandleExpr
    <$> resolveStmtTypes body
    <*> traverse resolveHandlerTypes handlers
resolveExprTypes (MatchExpr expr clauses) =
    MatchExpr
    <$> resolveExprTypes expr
    <*> traverse resolveMatchClauseTypes clauses

resolveMatchClauseTypes :: MonadTypeResolution m => MatchClause Desugared -> m (MatchClause TypeAnnotated)
resolveMatchClauseTypes (MatchClause pat stmt) = MatchClause <$> resolvePatternTypes pat <*> resolveStmtTypes stmt

resolvePatternTypes :: MonadTypeResolution m => Pattern Desugared -> m (Pattern TypeAnnotated)
resolvePatternTypes (ConstructorPattern name argPats) = ConstructorPattern name <$> traverse resolvePatternTypes argPats
resolvePatternTypes (WildcardPattern name t) = WildcardPattern name <$> traverse resolveTypeExpr t

resolveHandlerTypes :: MonadTypeResolution m => HandlerClause Desugared -> m (HandlerClause TypeAnnotated)
resolveHandlerTypes (HandlerClause name args rt body) =
    HandlerClause name
    <$> traverse (traverse (traverse resolveTypeExpr)) args
    <*> traverse resolveTypeExpr rt
    <*> resolveStmtTypes body


processTopLevel :: MonadTypeResolution m => [TopLevel Desugared] -> m ()
processTopLevel topLevelDecls = forM_ topLevelDecls $ \case
    ProductTypeDef typeName (ensureTypedArgs -> Just members) -> do
        resolvedMembers <- traverse (bitraverse pure resolveTypeExpr) members

        let declaredType = KUserType typeName
        let memberTypes  = snd <$> resolvedMembers

        -- Type
        modify $ addType typeName declaredType

        -- Constructor
        let constructorType = KFunc memberTypes PureEffect declaredType
        modify $ addBinding (Identifier typeName)
                            (Binding Constant [constructorType])

        -- Fields
        forM_ resolvedMembers $ \(fieldName, fieldType) -> do
            modify $ addFieldBinding declaredType fieldName fieldType

        -- Field accessors
        forM_ resolvedMembers $ \(fieldName, fieldType) -> do
            let accessorType = KFunc [declaredType] PureEffect fieldType
            modify $ addBinding (Accessor fieldName)
                                (Binding Constant [accessorType])
    ProductTypeDef{} -> throwError MissingFieldTypes

    SumTypeDef typeName (ensureTypedConstructors -> Just constructors) -> do
        let declaredType = KUserType typeName

        -- Type
        modify $ addType typeName declaredType

        -- Constructor functions
        forM_ constructors $ \(name, argTypeExprs) -> do
            argTypes <- traverse resolveTypeExpr argTypeExprs
            let constructorType = case argTypes of
                                  [] -> declaredType
                                  _ -> KFunc argTypes PureEffect declaredType

            modify $ addBinding (Identifier name)
                                (Binding Constant [constructorType])

            modify $ addConstructorBinding (declaredType, name) argTypes

    SumTypeDef{} -> throwError MissingFieldTypes


    FuncDef name (ensureTypedArgs -> Just args) effExpr (Just rtExpr) _body ->
        do
            argTypes <- mapM resolveTypeExpr (snd <$> args)
            rt       <- resolveTypeExpr rtExpr
            eff      <- resolveEffectExpr effExpr
            let funcType = KFunc argTypes eff rt
            modify (addBinding (Identifier name) (Binding Constant [funcType]))
    FuncDef _ (ensureTypedArgs -> Just _) _eff Nothing _body ->
        throwError MissingReturnType
    FuncDef{} -> throwError MissingArgumentTypes

    OperationDef name (ensureTypedArgs -> Just args) (Just rtExpr) -> do
        rt <- resolveTypeExpr rtExpr
        argTypes <- mapM resolveTypeExpr (snd <$> args)
        let declaredEffect = KEffect (Just name) [KOperation name argTypes rt]
        modify (addEffect name declaredEffect)
        modify (addBinding (Identifier name) (Binding Constant [KFunc argTypes declaredEffect rt]))

    OperationDef _ (ensureTypedArgs -> Just _) Nothing ->
        throwError MissingReturnType
    OperationDef{} -> throwError MissingArgumentTypes

    EffectSynonymDef name ops -> do
        (KEffect _ resolvedOps) <- resolveEffectExpr (EffectNames ops)
        modify (addEffect name (KEffect (Just name) resolvedOps))

resolveTypeExpr :: MonadTypeResolution m => TypeExpr -> m KType
resolveTypeExpr tExpr@(TypeName name) =
    gets (Map.lookup name . typeBindings) >>= \case
        Nothing -> throwError (TypeResolutionError tExpr)
        Just t  -> pure t
resolveTypeExpr (SignatureType argExprs effExpr rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    eff  <- resolveEffectExpr effExpr
    return (KFunc args eff rt)

resolveEffectExpr :: MonadTypeResolution m => ParsedEffect -> m KEffect
resolveEffectExpr (EffectNames names) = do
    effectBindings <- gets effectBindings

    resolved <- forM names $ \name ->
        case Map.lookup name effectBindings of
            Just eff -> pure eff
            Nothing  -> throwError (NonExistentEffect name)

    return (mconcat resolved)

ensureTypedConstructors :: [(name, [Maybe tExpr])] -> Maybe [(name, [tExpr])]
ensureTypedConstructors = traverse (traverse sequence)

ensureTypedArgs :: [(name, Maybe tExpr)] -> Maybe [(name, tExpr)]
ensureTypedArgs = traverse sequence
