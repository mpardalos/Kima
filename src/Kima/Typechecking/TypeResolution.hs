{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.TypeResolution where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.Types

type MonadTypeResolution m = (MonadState TypeCtx m, MonadError TypecheckingError m)

-- | Resolve all typeExprs in an AST.
-- | Note: in DataDefs, accessor types are annotated with the type of the attribute,
-- |       **not** their function type.
resolveTypes
    :: (MonadState TypeCtx m, MonadError TypecheckingError m) 
    => DesugaredAST p
    -> m (TypeAnnotatedAST p)
resolveTypes ast@Program{} = do 
    processDataDefs ast
    traverseTypeAnnotations resolveTypeExpr ast
resolveTypes ast = traverseTypeAnnotations resolveTypeExpr ast

processDataDefs :: MonadTypeResolution m => DesugaredAST 'Module -> m ()
processDataDefs (Program topLevelDecls) = forM_ topLevelDecls $ \case 
    DataDef typeName members -> do
        let declaredType = KUserType typeName
        -- Declares a new type
        modify (addType typeName declaredType)
        typedMembers <- traverse (traverse resolveTypeExpr) members

        -- Accessors
        forM_ typedMembers $ \(memberName, memberType) ->
            modify (addBinding (Identifier memberName)
                    (Binding Constant [KFunc ([declaredType] $-> memberType)]))

        -- And a constructor
        let memberTypes = snd <$> typedMembers
        modify (addBinding (Identifier typeName)
                (Binding Constant [KFunc (memberTypes $-> declaredType)]))

    FuncDef{} -> pure ()

resolveTypeExpr :: MonadTypeResolution m => TypeExpr -> m KType
resolveTypeExpr tExpr@(TypeName name) =
    gets (typeBindings >>> Map.lookup name) >>= \case
        Nothing -> throwError (TypeResolutionError tExpr)
        Just t  -> pure t
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return (KFunc (args $-> rt))
