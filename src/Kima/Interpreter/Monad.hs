module Kima.Interpreter.Monad where

import Control.Monad.Except
import Control.Monad.State
import Kima.Interpreter.Types
import Kima.Interpreter.Builtins

newtype Interpreter a = Interpreter { 
    runInterpreter :: ExceptT RuntimeError (
                      StateT (Environment Value) 
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

execInterpreter :: Interpreter a -> IO (Either RuntimeError a)
execInterpreter = (`evalStateT` baseEnv) . runExceptT . runInterpreter