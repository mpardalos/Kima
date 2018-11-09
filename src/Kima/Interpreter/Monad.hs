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


execInterpreter :: Interpreter a -> IO (Either RuntimeError a)
execInterpreter = (`evalStateT` baseEnv) . runExceptT . runInterpreter