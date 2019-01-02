module Kima.Typechecking.TypeResolution where

import Control.Monad.Except
import Kima.AST
import Kima.KimaTypes
import Kima.Typechecking.Types

resolveTypes
    :: MonadError TypecheckingError m
    => DesugaredAST p
    -> m (TypeAnnotatedAST p)
resolveTypes = traverseTypeAnnotations resolveTypeExpr

-- | Resolves a type expression. Since custom type don't exist yet, 
-- | just has a hardcoded list of types
resolveTypeExpr :: MonadError TypecheckingError m => TypeExpr -> m KType
resolveTypeExpr (TypeName "Int"   )             = pure KInt
resolveTypeExpr (TypeName "String")             = pure KString
resolveTypeExpr (TypeName "Float" )             = pure KFloat
resolveTypeExpr (TypeName "Bool"  )             = pure KBool
resolveTypeExpr (TypeName "Unit"  )             = pure KUnit
resolveTypeExpr tExpr@TypeName{} = throwError (TypeResolutionError tExpr)
resolveTypeExpr (SignatureType argExprs rtExpr) = do
    args <- traverse resolveTypeExpr argExprs
    rt   <- resolveTypeExpr rtExpr
    return (KFunc (args $-> rt))