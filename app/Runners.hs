{-|
Running Kima up to a certain stage.
-}

module Runners(dumpFileAtStage, runFile) where

import Control.Monad.Except
import OpenTelemetry.Eventlog

import Kima.Interface
import Kima.AST
import Kima.Interpreter as Interpreter
import Kima.Syntax
import Kima.Builtins

import ArgumentParser

-- | Take the code in a file up to a certain stage.
-- TypeApplications are probably necessary to use this effectively,
-- although it should work with normal type annotations as well
-- This:
-- > fromFileTo @Desugared "input.k"
-- reads the code from a file and desugars it
--
-- It is equivalent to
-- desugared :: AST 'Module Desugared <- fromFileTo "input.k"
fromFileTo :: forall to m. (MonadInterface m, TransformAST Module Parsed to) => FilePath -> m (Module to)
fromFileTo fn = do
    src <- withSpan_ "reading file" $ liftIO (readFile fn)
    parsedAST <- withSpan_ "parsing" $ runEither (runParser program fn src)
    transformAST parsedAST

runFile :: FilePath -> IO ()
runFile fn = do
    ast :: Module Runtime <- runUserInterface $ fromFileTo fn
    env <- refify (Interpreter.Environment baseEnv)
    Interpreter.execInterpreter env (Interpreter.runModule (TIdentifier "main" (KFunc [] ioEffect KUnit)) ast) >>= \case
        Right _ -> pure ()
        Left runtimeError -> putStrLn (userShow runtimeError)

dumpFileAtStage :: DumpStage -> FilePath -> IO ()
dumpFileAtStage stg fn = case stg of
    Parsed        -> runUserInterface (fromFileTo @Parsed fn) >>= print
    Desugared     -> runUserInterface (fromFileTo @Desugared fn) >>= print
    TypeAnnotated -> runUserInterface (fromFileTo @TypeAnnotated fn) >>= print
    Typed         -> runUserInterface (fromFileTo @Typed fn) >>= print
