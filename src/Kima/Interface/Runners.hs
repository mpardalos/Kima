{-|
Running Kima up to a certain stage.
-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Kima.Interface.Runners where

import Control.Monad.Except
import Control.Monad.State

import Kima.AST
import Kima.Builtins
import Kima.Desugar
import Kima.Interface.Types
import qualified Kima.Frontend as F
import qualified Kima.Interpreter as I
import qualified Kima.Typechecking as T

-- | Implements transformations from one AST type to another.
-- | We should infer/generate transitive instances.
-- | I.e. (TransformAST a b, TransformAST b c) => TransformAST a c
-- | TODO Generate transitive instances for TransformAST
-- | Until then, all transitive instances should be written by hand
class (ASTTag from, ASTTag to) => TransformAST from to where
    transformAST :: MonadInterface m => AST p from -> m (AST p to)

instance ASTTag a => TransformAST a a where
    transformAST = pure

instance TransformAST Parsed Desugared where
    transformAST = pure . desugar

instance TransformAST Parsed TypeAnnotated where
    transformAST ast = do
        desugaredAST :: AST p Desugared <- transformAST ast
        transformAST desugaredAST

instance TransformAST Parsed TVars where
    transformAST parsed = do
        typeAnnotated :: AST p TypeAnnotated <- transformAST parsed
        transformAST typeAnnotated

instance TransformAST Parsed Typed where
    transformAST = runEither
        . T.typecheck baseTypeCtx
        . desugar

instance TransformAST Desugared TypeAnnotated where
    transformAST = runEither
        . (`evalStateT` T.typeBindings baseTypeCtx)
        . T.resolveTypes

instance TransformAST TypeAnnotated TVars where
    transformAST = pure . T.addTVars

instance TransformAST Desugared Typed where
    transformAST = runEither . T.typecheck baseTypeCtx

-- | Take the code in a file up to a certain stage.
-- TypeApplications are probably necessary to use this effectively,
-- although it should work with normal type annotations as well
-- This:
-- > fromFileTo @Desugared "input.k"
-- reads the code from a file and desugars it
--
-- It is equivalent to
-- desugared :: AST 'Module Desugared <- fromFileTo "input.k"
fromFileTo :: forall to. TransformAST Parsed to => FilePath -> IO (AST 'Module to)
fromFileTo fn = runMonadInterface $ do
    src <- liftIO (readFile fn)
    parsedAST <- runEither (F.runParser F.program fn src)
    transformAST parsedAST

runFile :: FilePath -> IO I.Value
runFile fn = runMonadInterface $ do
    ast :: AST 'Module Runtime <- liftIO $ fromFileTo fn
    result <- liftIO $ I.run baseEnv ast
    runEither result

constraintFile :: FilePath -> IO T.EqConstraintSet
constraintFile = runMonadInterface . (liftIO . fromFileTo @Parsed >=> constraintAST)

constraintAST :: (MonadInterface m, TransformAST from TVars) => AST p from -> m T.EqConstraintSet
constraintAST =  fmap T.makeConstraints . transformAST

domainsOfFile :: FilePath -> IO T.Domains
domainsOfFile = runMonadInterface . (liftIO . fromFileTo @Parsed >=> domainsOfAST)

domainsOfAST :: (MonadInterface m, TransformAST from TVars) => AST p from -> m T.Domains
domainsOfAST =  runEither <=< (fmap (T.makeDomains baseTypeCtx) . transformAST)
