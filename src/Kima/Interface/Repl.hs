module Kima.Interface.Repl where

import Control.Arrow hiding (first, second)
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.IORef
import System.IO
import Text.Megaparsec

import Kima.Builtins
import Kima.Desugar
import Kima.Interpreter
import Kima.Typechecking
import Kima.Frontend

repl :: IO ()
repl = do
    hSetBuffering stdin LineBuffering
    typeCtxRef <- newIORef baseTypeCtx
    envRef     <- newIORef baseEnv
    forever $ do
        liftIO (putStr ">>> ")
        lineResult <- join $ runLine
            <$> readIORef typeCtxRef
            <*> readIORef envRef
            <*> getLine
        case lineResult of
            Left  err -> putStrLn err
            Right (value, newTypeCtx, newEnv) -> do
                writeIORef typeCtxRef newTypeCtx
                writeIORef envRef newEnv
                putStrLn ("> " <> show value)


runLine :: TypeCtx -> Environment Value -> String -> IO (Either String (Value, TypeCtx, Environment Value))
runLine typeCtx interpreterEnv input = runExceptT $ do
    -- Run up to typechecking, converting errors to strings (the parts after >>>)
    (typedAST, newTypeCtx) <- liftEither
        $   ( runParser stmt ""            >>> first errorBundlePretty ) input
        >>= ( desugar                      >>> pure                    )
        >>= ( typecheckWithTypeCtx typeCtx >>> first show              )
    -- Run the typedAST, lifting as needed
    (value, newEnv) <- liftEither =<< liftIO 
        (first show <$> runWithEnv interpreterEnv typedAST)
    return (value, newTypeCtx, newEnv)
