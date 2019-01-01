module Kima.Interpreter.Types where

import Prelude hiding (lookup)

import Kima.AST

import Control.Newtype.Generics

import Control.Monad.Except
import Control.Monad.State

import Data.Map hiding (toList, fromList)

import GHC.Generics

type RuntimeName = TypedName
type RuntimeAST p = TypedAST p
type RuntimeProgram = TypedProgram

type MonadRE m = (Monad m, MonadError RuntimeError m)
type MonadEnv m = (Monad m, MonadState (Environment Value) m)
type MonadInterpreter m = (MonadRE m, MonadEnv m, MonadConsole m)

data RuntimeError = RuntimeError
    deriving Show

runtimeError :: MonadRE m => m a
runtimeError = throwError RuntimeError

data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function [RuntimeName] (RuntimeAST 'Stmt)
           | BuiltinFunction1 (forall m. MonadInterpreter m => Value -> m Value)
           | BuiltinFunction2 (forall m. MonadInterpreter m => Value -> Value -> m Value)
           | BuiltinFunction3 (forall m. MonadInterpreter m => Value -> Value -> Value -> m Value)
           | Unit

class Monad m => MonadConsole m where
    consoleWrite :: String -> m ()
    consoleRead :: m String

newtype Environment a = Environment {unEnv :: Map RuntimeName a}
    deriving (Functor, Semigroup, Generic)
instance Newtype (Environment a)