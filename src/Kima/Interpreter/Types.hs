module Kima.Interpreter.Types where

import           Prelude                 hiding ( lookup )

import           Kima.AST

import           Control.Newtype.Generics

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Map                hiding ( toList
                                                , fromList
                                                )

import           GHC.Generics

type RuntimeName = TypedName
type RuntimeAST p = TypedAST p
type RuntimeProgram = TypedProgram

type MonadRE m = (Monad m, MonadError RuntimeError m)
type MonadEnv m = (Monad m, MonadState (Environment Value) m)
type MonadInterpreter m = (MonadRE m, MonadEnv m, MonadConsole m)

data RuntimeError = NotInScope RuntimeName
                  | WrongArgumentCount Int Int
                  | WrongConditionType
                  | NotAFunction Value
                  | BuiltinFunctionError String
    deriving Show

data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function [RuntimeName] (RuntimeAST 'Stmt)
           | BuiltinFunction0 (forall m. MonadInterpreter m => m Value)
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

instance Show Value where
    show (Integer v) = show v
    show (Float v) = show v
    show (Bool v) = show v
    show (String v) = show v
    show Function{} = "Function"
    show BuiltinFunction0{} = "Builtin function"
    show BuiltinFunction1{} = "Builtin function"
    show BuiltinFunction2{} = "Builtin function"
    show BuiltinFunction3{} = "Builtin function"
    show Unit = "()"
