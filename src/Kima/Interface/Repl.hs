module Kima.Interface.Repl where

import Control.Monad
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import System.IO

import Kima.Interface.Runners
import qualified Kima.Frontend as F
import Kima.Builtins
import Kima.Interpreter
import Kima.Typechecking
import Kima.Interpreter.Monad
import Text.Megaparsec

repl :: IO ()
repl = do
    hSetBuffering stdin LineBuffering
    interpreterStateRef <- newIORef baseEnv
    typecheckerStateRef <- newIORef baseTypeCtx
    forever $ do
        liftIO (putStr ">>> ")
        input <- getLine
        case F.runParser F.stmt "" input of
            Left err -> putStrLn (errorBundlePretty err)
            Right parseAST -> do 
                desugaredAST <- desugarAST parseAST 
                typecheckerState <- readIORef typecheckerStateRef
                case typecheckWithTypeCtx typecheckerState desugaredAST of
                    Left err -> putStrLn ("Type Error: " <> show err)
                    Right (typedAST, newTypecheckerState) -> do
                        writeIORef typecheckerStateRef newTypecheckerState
                        replState <- readIORef interpreterStateRef
                        (result, newState) <- (
                            runStmt >>> 
                            runInterpreter >>> 
                            runExceptT >>> 
                            (`runStateT` replState)) 
                            typedAST
                        case result of
                            Left err -> print err
                            Right val -> do
                                putStrLn ("> " <> show val)
                                writeIORef interpreterStateRef newState