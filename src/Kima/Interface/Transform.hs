{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Running Kima up to a certain stage.
-}

module Kima.Interface.Transform where

import GHC.TypeLits

import           Control.Monad.State

import           Kima.AST
import           Kima.Builtins
import           Kima.Desugar
import           Kima.Interface.Monad
import qualified Kima.Syntax                   as F
import qualified Kima.Types                    as T

import           OpenTelemetry.Eventlog

-- | Implements transformations from one AST type to another.
-- | We should infer/generate transitive instances.
-- | I.e. (TransformAST a b, TransformAST b c) => TransformAST a c
class (ASTTag from, ASTTag to) => TransformAST part from to where
    transformAST :: MonadInterface m => part from -> m (part to)

transitively1
    :: forall inter from to part m
     . ( MonadInterface m
       , TransformAST part from inter
       , TransformAST part inter to
       )
    => part from
    -> m (part to)
transitively1 ast = do
    intermediate :: part inter <- transformAST ast
    transformAST intermediate

instance ASTTag a => TransformAST Module a a where
    transformAST = pure
instance ASTTag a => TransformAST TopLevel a a where
    transformAST = pure
instance ASTTag a => TransformAST Stmt a a where
    transformAST = pure
instance ASTTag a => TransformAST Expr a a where
    transformAST = pure

instance TransformAST Module Parsed Desugared where
    transformAST = withSpan_ "desugaring" . pure . desugarModule
instance TransformAST TopLevel Parsed Desugared where
    transformAST = withSpan_ "desugaring" . pure . desugarTopLevel
instance TransformAST Stmt Parsed Desugared where
    transformAST = withSpan_ "desugaring" . pure . desugarStmt
instance TransformAST Expr Parsed Desugared where
    transformAST = withSpan_ "desugaring" . pure . desugarExpr

instance TransformAST Module Desugared TypeAnnotated where
    transformAST =
        withSpan_ "type resolution" .
        runEither . (`evalStateT` baseTypeCtx) . T.resolveModuleTypes
instance TransformAST TopLevel Desugared TypeAnnotated where
    transformAST =
        withSpan_ "type resolution" .
        runEither . (`evalStateT` baseTypeCtx) . T.resolveTopLevelTypes
instance TransformAST Stmt Desugared TypeAnnotated where
    transformAST =
        withSpan_ "type resolution" .
        runEither . (`evalStateT` baseTypeCtx) . T.resolveStmtTypes
instance TransformAST Expr Desugared TypeAnnotated where
    transformAST =
        withSpan_ "type resolution" .
        runEither . (`evalStateT` baseTypeCtx) . T.resolveExprTypes

instance
    TypeError ('Text "No instance for TransformAST " ':<>: 'ShowType part ':<>: 'Text " TypeAnnotated Typed" ':$$:
               'Text "You should use another instance like TransformAST " ':<>: 'ShowType part ':<>: 'Text " Parsed Typed" ':<>:
               'Text " or TransformAST " ':<>: 'ShowType part ':<>: 'Text " Desugared Typed")
    => TransformAST part TypeAnnotated Typed where
    transformAST = undefined

instance TransformAST Module Desugared Typed where
    transformAST = withSpan_ "typechecking" . runEither . T.typecheckModule baseTypeCtx
instance TransformAST TopLevel Desugared Typed where
    transformAST = withSpan_ "typechecking" . runEither . T.typecheckTopLevel baseTypeCtx
instance TransformAST Stmt Desugared Typed where
    transformAST = withSpan_ "typechecking" . runEither . T.typecheckStmt baseTypeCtx
instance TransformAST Expr Desugared Typed where
    transformAST = withSpan_ "typechecking" . runEither . T.typecheckExpr baseTypeCtx

instance TransformAST Module Parsed TypeAnnotated where
    transformAST = transitively1 @Desugared
instance TransformAST TopLevel Parsed TypeAnnotated where
    transformAST = transitively1 @Desugared
instance TransformAST Stmt Parsed TypeAnnotated where
    transformAST = transitively1 @Desugared
instance TransformAST Expr Parsed TypeAnnotated where
    transformAST = transitively1 @Desugared

instance TransformAST Module Parsed Typed where
    transformAST = transitively1 @Desugared
instance TransformAST TopLevel Parsed Typed where
    transformAST = transitively1 @Desugared
instance TransformAST Stmt Parsed Typed where
    transformAST = transitively1 @Desugared
instance TransformAST Expr Parsed Typed where
    transformAST = transitively1 @Desugared

-- | Take the code in a string up to a certain stage.
-- TypeApplications are probably necessary to use this effectively,
-- although it should work with normal type annotations as well
-- This:
-- > fromStringTo @Desugared codeStr
-- reads the code from the string and desugars it
--
-- It is equivalent to
-- desugared :: AST 'Module Desugared <- fromStringTo codeStr
--
fromStringTo :: forall to m. (MonadInterface m, TransformAST Module Parsed to) => String -> m (Module to)
fromStringTo src = do
    parsedAST <- runEither (F.runParser F.program "" src)
    transformAST parsedAST
