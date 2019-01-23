module Kima.Interpreter.Types where

import           Prelude                 hiding ( lookup )

import           Kima.AST

import           Control.Newtype.Generics

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Text.Prettyprint.Doc

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
                  | WrongConditionType Value
                  | NotAFunction Value
                  | BuiltinFunctionError String
    deriving Show

data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function [RuntimeName] (RuntimeAST 'Stmt)
           | BuiltinFunction (forall m. MonadInterpreter m => [Value] -> m Value)
           | Unit

class Monad m => MonadConsole m where
    consoleWrite :: String -> m ()
    consoleRead :: m String

newtype Environment a = Environment {unEnv :: Map RuntimeName a}
    deriving (Functor, Semigroup, Generic, Show)
instance Newtype (Environment a)

instance Show Value where
    show (Integer v) = show v
    show (Float v) = show v
    show (Bool v) = show v
    show (String v) = show v
    show Function{} = "Function"
    show BuiltinFunction{} = "Builtin function"
    show Unit = "()"

instance Pretty Value where
    pretty (Integer v)          = pretty v
    pretty (Float v)            = pretty v
    pretty (Bool v)             = pretty v
    pretty (String v)           = pretty v
    pretty (Function args body) =
        "fun" <+> tupled (pretty <$> args) <+> "{" 
        <> line
            <> indent 4 (pretty body)
        <> line <> "}"

    pretty BuiltinFunction{}   = "Builtin function"
    pretty Unit                 = "()"

instance Pretty RuntimeError where
    pretty ( NotInScope name                 ) =
        pretty name <+> "is not in scope"
    pretty ( WrongArgumentCount got expected ) =
        "Expected" <+> pretty expected <+> "args" <+>
        "but got" <+> pretty got
    pretty ( WrongConditionType v ) = 
        "Expected a boolean condition value but got" <+> pretty v
    pretty ( NotAFunction v                  ) =
        "Expected a function but got" <+> pretty v
    pretty ( BuiltinFunctionError err        ) = pretty err
