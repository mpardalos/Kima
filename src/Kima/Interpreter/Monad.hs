module Kima.Interpreter.Monad where

import Control.Monad.Except
import Control.Monad.State
import Kima.Interpreter.Types

newtype Interpreter a = Interpreter {
    unInterpreter :: StateT (Environment Value) (
                     ExceptT RuntimeError
                     IO) a
} deriving (
    Functor,
    Applicative,
    Monad,
    MonadError RuntimeError,
    MonadState (Environment Value),
    MonadIO)

instance MonadConsole Interpreter where
    consoleRead = liftIO getLine
    consoleWrite = liftIO . putStr

execInterpreter :: Environment Value -> Interpreter a -> IO (Either RuntimeError a)
execInterpreter env = runExceptT . (`evalStateT` env) . unInterpreter

runInterpreter :: Environment Value -> Interpreter a -> IO (Either RuntimeError (a, Environment Value))
runInterpreter env = runExceptT . (`runStateT` env) . unInterpreter