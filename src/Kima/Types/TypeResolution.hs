{-# LANGUAGE OverloadedLists #-}
module Kima.Types.TypeResolution where

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
resolveTypes
    :: MonadTypeResolution m => AST Desugared -> m (AST TypeAnnotated)
resolveTypes (ModuleAST ast) = do
    processTopLevel ast
    traverseFreeAnnotations (traverse resolveTypeExpr) (ModuleAST ast)
resolveTypes ast = traverseFreeAnnotations (traverse resolveTypeExpr) ast

processTopLevel :: MonadTypeResolution m => Module Desugared -> m ()
processTopLevel (Program topLevelDecls) = forM_ topLevelDecls $ \case
    DataDef typeName (ensureTypedArgs -> Just members) -> do
        resolvedMembers <- traverse (bitraverse pure resolveTypeExpr) members

        let declaredType = KUserType typeName resolvedMembers
        let memberTypes  = snd <$> resolvedMembers

        modify $ addType typeName declaredType

        let constructorType = KFunc memberTypes noEffect declaredType
        modify $ addBinding (Identifier typeName)
                            (Binding Constant [constructorType])

        forM_ resolvedMembers $ \(fieldName, fieldType) -> do
            let accessorType = KFunc [declaredType] noEffect fieldType
            modify $ addBinding (Accessor fieldName)
                                (Binding Constant [accessorType])
    DataDef{} -> throwError MissingFieldTypes

    FuncDef name (ensureTypedArgs -> Just args) eff (Just rtExpr) _body -> do
        argTypes <- mapM resolveTypeExpr (snd <$> args)
        rt       <- resolveTypeExpr rtExpr
        let funcType = KFunc argTypes eff rt
        modify (addBinding (Identifier name) (Binding Constant [funcType]))
    FuncDef _ (ensureTypedArgs -> Just _) _eff Nothing _body -> throwError MissingReturnType
    FuncDef{} -> throwError MissingArgumentTypes

resolveTypeExpr :: MonadTypeResolution m => TypeExpr -> m KType
resolveTypeExpr tExpr@(TypeName name) =
    gets (Map.lookup name . typeBindings) >>= \case
        Nothing -> throwError (TypeResolutionError tExpr)
        Just t  -> pure t
resolveTypeExpr (SignatureType argExprs eff rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return (KFunc args eff rt)

ensureTypedArgs :: [(a, Maybe TypeExpr)] -> Maybe [(a, TypeExpr)]
ensureTypedArgs = traverse sequence
