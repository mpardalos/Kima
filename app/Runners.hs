{-|
Running Kima up to a certain stage.
-}

module Runners where

import Control.Monad.Except

import Kima.Interface
import Kima.AST
import Kima.Interpreter as Interpreter
import Kima.Syntax
import Kima.Builtins

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
    parsedAST <- runEither (runParser program fn src)
    transformAST parsedAST

fromStringTo :: forall to m. (MonadInterface m, TransformAST Parsed to) => String -> m (AST 'Module to)
fromStringTo src = do
    parsedAST <- runEither (runParser program "" src)
    transformAST parsedAST

runFile :: FilePath -> IO ()
runFile fn = do
    ast :: AST 'Module Runtime <- fromFileTo fn
    void $ Interpreter.run (Interpreter.Environment baseEnv) ast
