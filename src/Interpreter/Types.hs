{-# LANGUAGE FlexibleContexts #-}

module Interpreter.Types where

import Prelude hiding (lookup)

import Control.Monad.State
import Control.Monad.Except

import Data.Map hiding (toList, fromList)

import AST

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

type MonadRE e m = (MonadError e m, e ~ RuntimeError)
type MonadEnv s m = (MonadState s m, s ~ Environment Value)

execInterpreter :: Interpreter a -> IO (Either RuntimeError a)
execInterpreter = (`evalStateT` mempty) . runExceptT . runInterpreter

data RuntimeError = RuntimeError

runtimeError :: MonadRE e m => m a
runtimeError = throwError RuntimeError

data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function [Name] Block
           | BuiltinFunction1 (Value -> Interpreter Value)
           | BuiltinFunction2 (Value -> Value -> Interpreter Value)
           | BuiltinFunction3 (Value -> Value -> Value -> Interpreter Value)
           | Unit

newtype Environment a = Environment {unEnv :: Map Name a}
    deriving (Functor, Monoid, Semigroup)

type EM e m = (MonadError e m, e ~ ())
type SM s m = (MonadState s m, s ~ Int)