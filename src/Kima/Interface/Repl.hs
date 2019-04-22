module Kima.Interface.Repl (ReplInterpreter, ReplState, repl) where

import           Control.Arrow           hiding ( first
                                                , second
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.IORef
import           System.IO
import           Safe
import           Text.Megaparsec
import           Data.Text.Prettyprint.Doc

import           Kima.Builtins
import           Kima.Desugar
import           Kima.Interpreter
import           Kima.Interpreter.Types
import           Kima.Interpreter.Monad
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
    hSetBuffering stdin LineBuffering
    replStateRef <- newIORef (ReplState baseTypeCtx baseEnv)
    forever $ do
        putStr ">>> "
        hFlush stdout -- Output is line-buffered so we explicitly flush here
        lineResult <- join $ runLine <$> readIORef replStateRef <*> getLine
        case lineResult of
            Left  err                   -> putStrLn ("Error: " <> err)
            Right (value, newReplState) -> do
                writeIORef replStateRef newReplState
                putStrLn ("> " <> show (pretty value))
  where
    runLine :: ReplState -> String -> IO (Either String (Value, ReplState))
    runLine ReplState { typeCtx, interpreterEnv } input = runExceptT $ do
        -- Run up to typechecking, converting errors to strings (the parts after >>>)
        (typedAST, newTypeCtx) <-
            liftEither
            $   (runParser stmt "" >>> first errorBundlePretty) input
            >>= (desugar >>> pure)
            >>= (typecheckWithTypeCtx typeCtx >>> first (show . pretty))
        -- Run the typedAST, lifting as needed
        (value, newEnv) <- liftEither =<< liftIO
            (   first (show . pretty)
            <$> runInterpreter interpreterEnv (unReplInterpreter . runAST $ typedAST)
            )
        return (value, ReplState newTypeCtx newEnv)
