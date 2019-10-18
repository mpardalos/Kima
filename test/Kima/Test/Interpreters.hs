{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Kima.Test.Interpreters where

import           Kima.Interpreter.Types
import           Kima.Interpreter.Interpreter
import           Kima.Interface
import           Kima.Builtins
import           Kima.AST
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Functor
import           Data.Function
import           Data.IORef.Class
import           Test.Hspec

newtype TestInterpreter a = MockInterpreter {
        runInterpreter
                :: StateT (Environment (IORef Value)) (
                   ReaderT String (
                   WriterT String (
                   ExceptT RuntimeError
                   IO))) a
}
    deriving newtype (
        Functor, Applicative, Monad,
        MonadError RuntimeError,
        MonadReader String,
        MonadState (Environment (IORef Value)),
        MonadWriter String,
        MonadIO)
    deriving anyclass MonadIORef

newtype TestInterface a = TestInterface {
    runTestInterface :: ExceptT UserThrowableError IO a
} deriving newtype (
    Functor, Applicative, Monad,
    MonadError UserThrowableError,
    MonadIO)

instance MonadInterface TestInterface where
    userThrow = throwError . UserThrowableError

shouldFail :: TestInterface a -> Expectation
shouldFail action = runExceptT (runTestInterface action) >>= \case
    Left{}  -> pure ()
    Right{} -> expectationFailure "Expected a failure but got a result"

shouldRun :: TestInterface a -> Expectation
shouldRun action = runExceptT (runTestInterface action) >>= \case
    (Left err) ->
        expectationFailure ("Expected a result but failed with: \n" <> show err)
    Right{} -> pure ()

shouldRunWithInputOutput :: AST p Runtime -> String -> Maybe String -> Expectation
shouldRunWithInputOutput ast input maybeExpectedOutput = do
    (_, output) <- runInTestInterpreterWithInput input ast
    case maybeExpectedOutput of
        Nothing -> pure ()
        Just expectedOutput
            | output == expectedOutput -> pure ()
            | otherwise -> expectationFailure
                (  "Expected output: \n"
                <> expectedOutput
                <> "\nBut got: \n"
                <> output
                )

runInTestInterpreter :: MonadInterface m => AST p Runtime -> m (Value, String)
runInTestInterpreter = runInTestInterpreterWithInput ""

runInTestInterpreterWithInput
    :: MonadInterface m => String -> AST p Runtime -> m (Value, String)
runInTestInterpreterWithInput input inAST = do
    refEnv <- liftIO $ traverse newIORef (Environment baseEnv)
    result <-
        liftIO
        $ inAST
        & runExceptT
        . runWriterT
        . (`runReaderT` input)
        . (`evalStateT` refEnv)
        . Kima.Test.Interpreters.runInterpreter
        . \case
              ProgramAST  ast  -> runProgram ast $> Unit
              TopLevelAST ast  -> bindTopLevel ast
              StmtAST     stmt -> runStmt stmt
              ExprAST     expr -> evalExpr expr

    case result of
        Left  err -> userThrow err
        Right val -> return val

instance MonadConsole TestInterpreter where
    consoleRead  = ask
    consoleWrite = tell
