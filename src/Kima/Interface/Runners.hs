{-|
Running Kima up to a certain stage.
-}

module Kima.Interface.Runners where

import Control.Monad.Except
import Control.Monad.State

import Kima.AST
import Kima.Builtins
import Kima.Desugar
import Kima.Interface.Types
import qualified Kima.Syntax as F
import qualified Kima.Interpreter as I
import qualified Kima.Types as T

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

instance TransformAST Parsed Typed where
    transformAST = runEither
        . T.typecheck baseTypeCtx
        . desugar

instance TransformAST Desugared TypeAnnotated where
    transformAST = runEither
        . (`evalStateT` baseTypeCtx)
        . T.resolveTypes

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
fromFileTo :: forall to m. (MonadInterface m, TransformAST Parsed to) => FilePath -> m (AST 'Module to)
fromFileTo fn = do
    src <- liftIO (readFile fn)
    parsedAST <- runEither (F.runParser F.program fn src)
    transformAST parsedAST

fromStringTo :: forall to m. (MonadInterface m, TransformAST Parsed to) => String -> m (AST 'Module to)
fromStringTo src = do
    parsedAST <- runEither (F.runParser F.program "" src)
    transformAST parsedAST

runFile :: FilePath -> IO ()
runFile fn = do
    ast :: AST 'Module Runtime <- fromFileTo fn
    void $ I.run (I.Environment baseEnv) ast
