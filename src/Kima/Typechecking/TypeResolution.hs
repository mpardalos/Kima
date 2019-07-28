{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.TypeResolution where

import Control.Monad.Except
import Control.Monad.State
import Data.Bitraversable
import           Data.Map   (Map)
import qualified Data.Map as Map
import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.Errors

type MonadTypeResolution m = (MonadState (Map TypeName KType) m, MonadError TypecheckingError m)

-- | Resolve all typeExprs in an AST.
-- Note: in DataDefs, accessor types are annotated with the type of the attribute,
--       **not** their function type.
resolveTypes :: MonadTypeResolution m => AST p Desugared -> m (AST p TypeAnnotated)
resolveTypes ast@Program{} = do
    processDataDefs ast
    traverseFreeAnnotations resolveTypeExpr ast
resolveTypes ast = traverseFreeAnnotations resolveTypeExpr ast

processDataDefs
    :: MonadTypeResolution m => AST 'Module Desugared -> m ()
processDataDefs (Program topLevelDecls) = forM_ topLevelDecls $ \case
    DataDef typeName members -> do
        resolvedMembers <- traverse @[] (bitraverse @(,) pure resolveTypeExpr) members
        modify (Map.insert typeName (KUserType typeName resolvedMembers))
    FuncDef{} -> pure ()

resolveTypeExpr :: MonadTypeResolution m => TypeExpr -> m KType
resolveTypeExpr tExpr@(TypeName name) =
    gets (Map.lookup name) >>= \case
        Nothing -> throwError (TypeResolutionError tExpr)
        Just t  -> pure t
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return (KFunc (args $-> rt))
