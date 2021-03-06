{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Kima.Test.Interpreters where

import           Kima.Interpreter
import           Kima.Interpreter.Types
import           Kima.Interface
import           Kima.Builtins
import           Kima.AST
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Monad.Catch
import           Data.Function
import           Data.IORef.Class
import           Test.Hspec
import           Data.Bifunctor

newtype TestInterpreter a = MockInterpreter {
        runInterpreter
                :: ReaderT String (
                   StateT (Environment (IORef Value)) (
                   ExceptT RuntimeError (
                   WriterT String
                   IO))) a
}
    deriving newtype (
        Functor, Applicative, Monad,
        MonadError RuntimeError,
        MonadReader String,
        MonadState (Environment (IORef Value)),
        MonadWriter String,
        MonadIO,
        MonadThrow, MonadCatch, MonadMask)
    deriving anyclass MonadIORef

newtype TestInterface a = TestInterface {
    runTestInterface :: ExceptT UserThrowableError IO a
} deriving newtype (
    Functor, Applicative, Monad,
    MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadError UserThrowableError TestInterface where
    throwError e = TestInterface (ExceptT (pure (Left e)))
    catchError (TestInterface (ExceptT action)) handler =
        TestInterface $ ExceptT $ action >>= \case
            Left err -> runExceptT . runTestInterface $ handler err
            value    -> pure value

instance MonadInterface TestInterface where
    userThrow err = throwError (UserThrowableError err)

execTestInterface :: TestInterface a -> IO (Either UserThrowableError a)
execTestInterface action = runExceptT (runTestInterface action)

shouldFail :: TestInterface a -> Expectation
shouldFail action = runExceptT (runTestInterface action) >>= \case
    Left{}  -> pure ()
    Right{} -> expectationFailure "Expected a failure but got a result"

shouldRun :: TestInterface a -> Expectation
shouldRun action = runExceptT (runTestInterface action) >>= \case
    (Left err) ->
        expectationFailure ("Expected a result but failed with: \n" <> show err)
    Right{} -> pure ()

expectOutput :: Maybe String -> String -> Expectation
expectOutput (Just expectedOutput) output | expectedOutput /= output =
    expectationFailure
        ("Expected output: \n" <> expectedOutput <> "\nBut got: \n" <> output)
expectOutput _ _ = pure ()


runModule :: Module Runtime -> TestInterface String
runModule = runModuleWithInput ""

runModuleWithInput :: String -> Module Runtime -> TestInterface String
runModuleWithInput input inAST = do
    refEnv <- liftIO $ refify (Environment baseEnv)

    (hasError, output) <-
        inAST
            & Kima.Interpreter.runModule (TIdentifier "main" (KFunc [] ioEffect KUnit))
            & Kima.Test.Interpreters.runInterpreter
            & (`runReaderT` input)
            & (`evalStateT` refEnv)
            & mapExceptT (fmap (first UserThrowableError))
            & runExceptT
            & runWriterT
            & liftIO

    case hasError of
        Left err -> throwError err
        Right () -> return output

instance MonadConsole TestInterpreter where
    consoleRead  = ask
    consoleWrite = tell
