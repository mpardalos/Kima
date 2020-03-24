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
import           Data.Maybe
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
resolveModuleTypes (Program decls) = do
    processTopLevel decls
    Program <$> traverse resolveTopLevelTypes decls

resolveTopLevelTypes
    :: MonadTypeResolution m => TopLevel Desugared -> m (TopLevel TypeAnnotated)
resolveTopLevelTypes (FuncDef name argExprs effExpr rtExpr body) =
    FuncDef name
        <$> traverse (traverse (traverse resolveTypeExpr)) argExprs
        <*> resolveEffectExpr effExpr
        <*> traverse resolveTypeExpr rtExpr
        <*> resolveStmtTypes body
resolveTopLevelTypes (DataDef name args) =
    DataDef name <$> traverse (traverse (traverse resolveTypeExpr)) args
resolveTopLevelTypes (OperationDef name argExprs rtExpr ) =
    OperationDef name
        <$> traverse (traverse (traverse resolveTypeExpr)) argExprs
        <*> traverse resolveTypeExpr rtExpr
resolveTopLevelTypes (EffectSynonymDef name ops) = pure (EffectSynonymDef name ops)

resolveStmtTypes
    :: MonadTypeResolution m => Stmt Desugared -> m (Stmt TypeAnnotated)
resolveStmtTypes (ExprStmt expr ) = ExprStmt <$> resolveExprTypes expr
resolveStmtTypes (Block    stmts) = Block <$> traverse resolveStmtTypes stmts
resolveStmtTypes (While stmt) =
    While <$> bitraverse resolveExprTypes resolveStmtTypes stmt
resolveStmtTypes (If stmt) =
    If <$> bitraverse resolveExprTypes resolveStmtTypes stmt
resolveStmtTypes (Assign access expr) = Assign access <$> resolveExprTypes expr
resolveStmtTypes (Var name t expr) =
    Var name <$> traverse resolveTypeExpr t <*> resolveExprTypes expr
resolveStmtTypes (Let name t expr) =
    Let name <$> traverse resolveTypeExpr t <*> resolveExprTypes expr

resolveExprTypes
    :: MonadTypeResolution m => Expr Desugared -> m (Expr TypeAnnotated)
resolveExprTypes (FuncExpr argExprs effExpr rtExpr body) =
    FuncExpr
        <$> traverse (traverse (traverse resolveTypeExpr)) argExprs
        <*> resolveEffectExpr effExpr
        <*> traverse resolveTypeExpr rtExpr
        <*> resolveStmtTypes body
resolveExprTypes (Call callee args) =
    Call <$> resolveExprTypes callee <*> traverse resolveExprTypes args
resolveExprTypes (LiteralE    lit ) = pure (LiteralE lit)
resolveExprTypes (IdentifierE name) = pure (IdentifierE name)

processTopLevel :: MonadTypeResolution m => [TopLevel Desugared] -> m ()
processTopLevel topLevelDecls = forM_ topLevelDecls $ \case
    DataDef typeName (ensureTypedArgs -> Just members) -> do
        resolvedMembers <- traverse (bitraverse pure resolveTypeExpr) members

        let declaredType = KUserType typeName resolvedMembers
        let memberTypes  = snd <$> resolvedMembers

        modify $ addType typeName declaredType

        let constructorType = KFunc memberTypes PureEffect declaredType
        modify $ addBinding (Identifier typeName)
                            (Binding Constant [constructorType])

        forM_ resolvedMembers $ \(fieldName, fieldType) -> do
            let accessorType = KFunc [declaredType] PureEffect fieldType
            modify $ addBinding (Accessor fieldName)
                                (Binding Constant [accessorType])
    DataDef{} -> throwError MissingFieldTypes

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
    return (mconcat (mapMaybe (`Map.lookup` effectBindings) names))

ensureTypedArgs :: [(a, Maybe TypeExpr)] -> Maybe [(a, TypeExpr)]
ensureTypedArgs = traverse sequence
