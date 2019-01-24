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
    DataDefAnn (Name typeName) members -> do
        let declaredType = KUserType typeName
        -- Declares a new type
        modify (addType typeName declaredType)

        -- Accessors for its members
        forM_ members $ \(memberName, memberTypeExpr) -> do
            memberType <- resolveTypeExpr memberTypeExpr
            boundName <- case memberName of
                name@Accessor{} -> pure name
                name            -> throwError (UnexpectedNameType name)
            modify (addBinding boundName
                (Binding Constant [KFunc ([declaredType] $-> memberType)]))

        -- And a constructor
        memberTypes <- forM (snd <$> members) resolveTypeExpr
        modify (addBinding (Name typeName) (Binding Constant [
            KFunc (memberTypes $-> declaredType)]))

    DataDefAnn n _ -> throwError (UnexpectedNameType n)
    FuncDefAnn{} -> pure ()

resolveTypeExpr :: MonadTypeResolution m => TypeExpr -> m KType
resolveTypeExpr tExpr@(TypeName name) =
    gets (typeBindings >>> Map.lookup name) >>= \case
        Nothing -> throwError (TypeResolutionError tExpr)
        Just t  -> pure t
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return (KFunc (args $-> rt))
