{-# LANGUAGE OverloadedLists #-}
module Kima.Typechecking.TypeResolution where

import Control.Monad.Except
import Control.Monad.State
import           Data.Map   (Map)
import qualified Data.Map as Map
import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.Types

type MonadTypeResolution m = (MonadState (Map TypeName KType) m, MonadError TypecheckingError m)

-- | Resolve all typeExprs in an AST.
-- Note: in DataDefs, accessor types are annotated with the type of the attribute,
--       **not** their function type.
resolveTypes
    :: MonadTypeResolution m
    => DesugaredAST p
    -> m (TypeAnnotatedAST p)
resolveTypes ast@Program{} = do 
    processDataDefs ast
    traverseTypeAnnotations resolveTypeExpr ast
resolveTypes ast = traverseTypeAnnotations resolveTypeExpr ast

processDataDefs :: MonadTypeResolution m => DesugaredAST 'Module -> m ()
processDataDefs (Program topLevelDecls) = forM_ topLevelDecls $ \case 
    DataDef typeName _members -> modify (Map.insert typeName (KUserType typeName))
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
