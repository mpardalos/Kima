{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.TypeResolution where

import Control.Monad.Except
import Control.Monad.State
import Data.Bitraversable
import qualified Data.Map as Map
import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.TypeCtx
import Kima.Typechecking.Errors

type MonadTypeResolution m = (MonadState TypeCtx m, MonadError TypecheckingError m)

-- | Resolve all typeExprs in an AST.
-- Note: in DataDefs, accessor types are annotated with the type of the attribute,
--       **not** their function type.
resolveTypes :: MonadTypeResolution m => AST p Desugared -> m (AST p TypeAnnotated)
resolveTypes ast@Program{} = do
    processTopLevel ast
    traverseFreeAnnotations resolveTypeExpr ast
resolveTypes ast = traverseFreeAnnotations resolveTypeExpr ast

processTopLevel
    :: MonadTypeResolution m => AST 'Module Desugared -> m ()
processTopLevel (Program topLevelDecls) = forM_ topLevelDecls $ \case
    DataDef typeName members -> do
        resolvedMembers <- traverse @[]
            (bitraverse @(,) pure resolveTypeExpr)
            members

        let declaredType = KUserType typeName resolvedMembers
        let memberTypes  = snd <$> resolvedMembers

        modify $ addType typeName declaredType

        let constructorType = KFunc (memberTypes $-> declaredType)
        modify $ addBinding (Identifier typeName)
                            (Binding Constant [constructorType])

        forM_ resolvedMembers $ \(fieldName, fieldType) -> do
            let accessorType = KFunc ([declaredType] $-> fieldType)
            modify $ addBinding (Accessor fieldName)
                                (Binding Constant [accessorType])

    FuncDef name args rtExpr _body -> do
        argTypes <- mapM resolveTypeExpr (snd <$> args)
        rt <- resolveTypeExpr rtExpr
        let funcType = KFunc (argTypes $-> rt)
        modify (addBinding (Identifier name) (Binding Constant [funcType]))

resolveTypeExpr :: MonadTypeResolution m => TypeExpr -> m KType
resolveTypeExpr tExpr@(TypeName name) =
    gets (Map.lookup name . typeBindings) >>= \case
        Nothing -> throwError (TypeResolutionError tExpr)
        Just t  -> pure t
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return (KFunc (args $-> rt))
