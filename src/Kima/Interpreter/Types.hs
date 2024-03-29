module Kima.Interpreter.Types where

import           Prelude                 hiding ( lookup )

import           Kima.AST

import           Control.Monad.Except
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Text.Prettyprint.Doc

import           Data.Map                hiding ( toList
                                                , fromList
                                                )
import           Data.IORef.Class

import           GHC.Generics
import           GHC.Exts
import           System.IO

type RuntimeIdentifier = Identifier ('Annotation KType)

-- | There is circular dependencies around the following so we can't split them

-- | ---------- Values ----------------
data Value = Integer Integer
           | Float Double
           | Bool Bool
           | String String
           | Function Name [RuntimeIdentifier] (Stmt Runtime) (Environment (IORef Value))
           | BuiltinFunction (forall m. MonadInterpreter m => [Value] -> m Value)
           | ProductData Name [Value]
           | SumData Name [Value]
           | AccessorIdx Name Int -- | Just gives the index of the accessed value
           | Unit

newtype Environment a = Environment {unEnv :: Map RuntimeIdentifier a}
    deriving (Functor, Foldable, Traversable, Semigroup, Generic, Show, IsList)

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
    pretty (Function name _ _ _) = pretty name
    pretty BuiltinFunction{}   = "Builtin function"
    pretty (ProductData name vals)  = pretty name <+> "{" <> line <>
        indent 4 (vcat (pretty <$> vals))
        <> line <> "}"
    pretty (SumData constructorName vals) =
        pretty constructorName <> tupled (pretty <$> vals)
    pretty Unit                = "()"

-- | ---------- Errors ----------------
data RuntimeError = NotInScope RuntimeIdentifier
                  | WrongArgumentCount Int Int
                  | WrongConditionType Value
                  | NotAFunction Value
                  | BuiltinFunctionError String
                  | BreakError Value
                  | PatternMatchFailure Value
    deriving Show

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
    pretty ( PatternMatchFailure val         ) =
        "Pattern matching on" <+> pretty val <+> ": no pattern matched"
    pretty ( BreakError _ ) = "Uncaught BreakError"

-- | ---------- Execution ----------------
type MonadRE m = (Monad m, MonadError RuntimeError m)
type MonadEnv m = (Monad m, MonadState (Environment (IORef Value)) m, MonadIORef m)
type MonadInterpreter m = (MonadRE m, MonadEnv m, MonadConsole m, MonadIO m, MonadMask m)

newtype Interpreter a = Interpreter {
    unInterpreter :: StateT (Environment (IORef Value)) (
                     ExceptT RuntimeError
                     IO) a
} deriving (
    Functor,
    Applicative,
    Monad,
    MonadError RuntimeError,
    MonadState (Environment (IORef Value)),
    MonadIO,
    MonadMask, MonadCatch, MonadThrow)

class Monad m => MonadConsole m where
    consoleWrite :: String -> m ()
    consoleRead :: m String

instance MonadConsole Interpreter where
    consoleRead = liftIO getLine
    consoleWrite str = liftIO $ do
        putStr str
        hFlush stdout

instance MonadIORef Interpreter

execInterpreter
    :: Environment (IORef Value)
    -> Interpreter a
    -> IO (Either RuntimeError a)
execInterpreter env = runExceptT . (`evalStateT` env) . unInterpreter

runInterpreter
    :: Environment (IORef Value)
    -> Interpreter a
    -> IO (Either RuntimeError (a, Environment (IORef Value)))
runInterpreter env = runExceptT . (`runStateT` env) . unInterpreter
