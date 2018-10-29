module Interpreter.Types where

import Prelude hiding (lookup)

import AST

import Control.Monad.Except
import Control.Monad.State

import Data.Comp.Algebra
import Data.Map hiding (toList, fromList)


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

type EvalAlg f = Alg f Value
type EvalAlgM m f = AlgM m f Value
type EvalRAlgM m f = RAlgM m f Value

data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function [Name] DesugaredStmt
           | BuiltinFunction1 (Value -> Interpreter Value)
           | BuiltinFunction2 (Value -> Value -> Interpreter Value)
           | BuiltinFunction3 (Value -> Value -> Value -> Interpreter Value)
           | Unit

newtype Environment a = Environment {unEnv :: Map Name a}
    deriving (Functor, Monoid, Semigroup)