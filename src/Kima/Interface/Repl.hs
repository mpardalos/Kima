module Kima.Interface.Repl (ReplInterpreter, ReplState, repl) where

import           Control.Arrow           hiding ( first
                                                , second
                                                )
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Functor
import           Data.IORef
import           Safe
import           Text.Megaparsec
import           Data.Text.Prettyprint.Doc
import           System.Console.Haskeline

import           Kima.Builtins
import           Kima.Desugar
import           Kima.Interpreter
import           Kima.Interpreter.Types
import           Kima.Typechecking
import           Kima.Frontend

-- | newtype wrapper around Interpreter whose MonadConsole makes sure
-- | that any printed text ends with a newline
newtype ReplInterpreter a = ReplInterpreter {
    unReplInterpreter :: Interpreter a
} deriving (
    Functor,
    Applicative,
    Monad,
    MonadError RuntimeError,
    MonadState (Environment Value),
    MonadIO)

instance MonadConsole ReplInterpreter where
    consoleRead  = liftIO getLine
    consoleWrite = liftIO . putStr . \s -> case lastMay s of
        Just '\n' -> s
        _         -> s ++ "\n"

-- | Combines all necessary persistent data in the repl
data ReplState = ReplState {
    typeCtx :: TypeCtx,
    interpreterEnv :: Environment Value
}

repl :: IO ()
repl = do
    replStateRef <- newIORef (ReplState baseTypeCtx baseEnv)
    runInputT defaultSettings (mainloop replStateRef)
  where
    -- | The main loop of the repl. Quits only when EOF is received from haskeline
    mainloop :: IORef ReplState -> InputT IO ()
    mainloop replStateRef = getInputLine ">>> " >>= \case
        Nothing        -> return ()
        Just inputLine -> do
            replState <- lift $ readIORef replStateRef
            runLine replState inputLine >>= \case
                Left  err                   -> outputStrLn ("Error: " <> err)
                Right (value, newReplState) -> do
                    lift $ writeIORef replStateRef newReplState
                    outputStrLn ("> " <> show (pretty value))
            mainloop replStateRef

runLine :: MonadIO m => ReplState -> String -> m (Either String (Value, ReplState))
runLine ReplState { typeCtx, interpreterEnv } input = runExceptT $ do
    -- Run up to typechecking, converting errors to strings (the parts after >>>)
    (typedAST, newTypeCtx) <-
        liftEither
        $   (runParser stmt "" >>> first errorBundlePretty) input
        <&> desugar
        >>= (typecheckWithTypeCtx typeCtx >>> first (show . pretty))
    -- Run the typedAST, lifting as needed
    (value, newEnv) <- liftEither =<< liftIO
        (   first (show . pretty)
        <$> runInterpreter interpreterEnv (unReplInterpreter . runAST $ typedAST)
        )
    return (value, ReplState newTypeCtx newEnv)
