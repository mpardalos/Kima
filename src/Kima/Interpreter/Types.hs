module Kima.Interpreter.Types where

import           Prelude                 hiding ( lookup )

import           Kima.AST
import           Kima.KimaTypes

import           Control.Newtype.Generics

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Text.Prettyprint.Doc

import           Data.Map                hiding ( toList
                                                , fromList
                                                )

import           GHC.Generics
import           GHC.Exts

type RuntimeIdentifier = Identifier ('Annotation KType)

type MonadRE m = (Monad m, MonadError RuntimeError m)
type MonadEnv m = (Monad m, MonadState (Environment Value) m)
type MonadInterpreter m = (MonadRE m, MonadEnv m, MonadConsole m)

data RuntimeError = NotInScope RuntimeIdentifier
                  | WrongArgumentCount Int Int
                  | WrongConditionType Value
                  | NotAFunction Value
                  | BuiltinFunctionError String
    deriving Show

data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function [RuntimeIdentifier] (RuntimeAST 'Stmt)
           | BuiltinFunction (forall m. MonadInterpreter m => [Value] -> m Value)
           | ProductData [Value]
           | AccessorIdx Name Int -- | Just gives the index of the accessed value
           | Unit

class Monad m => MonadConsole m where
    consoleWrite :: String -> m ()
    consoleRead :: m String

newtype Environment a = Environment {unEnv :: Map RuntimeIdentifier a}
    deriving (Functor, Semigroup, Generic, Show)
instance Newtype (Environment a)

instance Pretty a => Pretty (Environment a) where
    pretty (Environment envMap) = vcat (
      (\(name, val) -> pretty name <> ": " <> pretty val)
      <$> toList envMap)

instance Show Value where
    show = show . pretty

instance Pretty Value where
    pretty (Integer v)          = pretty v
    pretty (Float v)            = pretty v
    pretty (Bool v)             = pretty v
    pretty (String v)           = pretty v
    pretty (AccessorIdx name _) = "{." <> pretty name <> "}"
    pretty (Function args body) =
        "fun" <+> tupled (pretty <$> args) <+> "{" 
        <> line
            <> indent 4 (pretty body)
        <> line <> "}"

    pretty BuiltinFunction{}   = "Builtin function"
    pretty (ProductData vals)  = "data {" <> line <>
        indent 4 (vcat (pretty <$> vals))
        <> line <> "}"
    pretty Unit                = "()"

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
