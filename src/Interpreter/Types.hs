module Interpreter.Types where

import Prelude hiding (lookup)

import AST

import Control.Newtype.Generics

import Control.Monad.Except
import Control.Monad.State

import Data.Comp.Algebra
import Data.Map hiding (toList, fromList)

import GHC.Generics

type MonadRE m = (Monad m, MonadError RuntimeError m)
type MonadEnv m = (Monad m, MonadState (Environment Value) m)
type MonadInterpreter m = (MonadRE m, MonadEnv m, MonadIO m)


data RuntimeError = RuntimeError
    deriving Show

runtimeError :: MonadRE m => m a
runtimeError = throwError RuntimeError

type EvalAlg f = Alg f Value
type EvalAlgM m f = AlgM m f Value
type EvalRAlgM m f = RAlgM m f Value

data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function [Name] DesugaredStmt
           | BuiltinFunction1 (forall m. MonadInterpreter m => Value -> m Value)
           | BuiltinFunction2 (forall m. MonadInterpreter m => Value -> Value -> m Value)
           | BuiltinFunction3 (forall m. MonadInterpreter m => Value -> Value -> Value -> m Value)
           | Unit

newtype Environment a = Environment {unEnv :: Map Name a}
    deriving (Functor, Semigroup, Generic)
instance Newtype (Environment a)