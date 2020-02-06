{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Running Kima up to a certain stage.
-}

module Kima.Interface.Transform where

import Control.Monad.State

import Kima.AST
import Kima.Builtins
import Kima.Desugar
import Kima.Interface.Monad
import qualified Kima.Syntax as F
import qualified Kima.Types as T

-- | Implements transformations from one AST type to another.
-- | We should infer/generate transitive instances.
-- | I.e. (TransformAST a b, TransformAST b c) => TransformAST a c
-- | TODO Generate transitive instances for TransformAST
-- | Until then, all transitive instances should be written by hand
class (ASTTag from, ASTTag to) => TransformAST part from to where
    transformAST :: MonadInterface m => part from -> m (part to)

transitively1 :: forall inter from to part m.
    ( MonadInterface m
    , TransformAST part from inter
    , TransformAST part inter to
    ) => part from -> m (part to)
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
    transformAST = pure . desugarModule
instance TransformAST TopLevel Parsed Desugared where
    transformAST = pure . desugarTopLevel
instance TransformAST Stmt Parsed Desugared where
    transformAST = pure . desugarStmt
instance TransformAST Expr Parsed Desugared where
    transformAST = pure . desugarExpr

instance TransformAST Module Desugared TypeAnnotated where
    transformAST = runEither
        . (`evalStateT` baseTypeCtx)
        . T.resolveModuleTypes
instance TransformAST TopLevel Desugared TypeAnnotated where
    transformAST = runEither
        . (`evalStateT` baseTypeCtx)
        . T.resolveTopLevelTypes
instance TransformAST Stmt Desugared TypeAnnotated where
    transformAST = runEither
        . (`evalStateT` baseTypeCtx)
        . T.resolveStmtTypes
instance TransformAST Expr Desugared TypeAnnotated where
    transformAST = runEither
        . (`evalStateT` baseTypeCtx)
        . T.resolveExprTypes

instance TransformAST Module Desugared Typed where
    transformAST = runEither . T.typecheckModule baseTypeCtx
instance TransformAST TopLevel Desugared Typed where
    transformAST = runEither . T.typecheckTopLevel baseTypeCtx
instance TransformAST Stmt Desugared Typed where
    transformAST = runEither . T.typecheckStmt baseTypeCtx
instance TransformAST Expr Desugared Typed where
    transformAST = runEither . T.typecheckExpr baseTypeCtx

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
