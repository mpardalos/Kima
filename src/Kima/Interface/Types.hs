module Kima.Interface.Types where

import Control.Arrow hiding (first)
import Control.Monad.Except

import Kima.Frontend
import Kima.Interpreter

class UserThrowable err where
    userShow :: err -> String

    default userShow :: Show err => err -> String
    userShow = show

instance UserThrowable ParseError where
    userShow = parseErrorPretty

-- instance Show t => UserThrowable (TypeError t)
instance UserThrowable RuntimeError

data UserThrowableError = forall err. UserThrowable err => UserThrowableError err
instance Show UserThrowableError where
    show (UserThrowableError err) = userShow err

newtype InterfaceM a = InterfaceM {
    runInterfaceM :: ExceptT UserThrowableError IO a
} deriving (
    Functor, Applicative, Monad,
    MonadError UserThrowableError, MonadIO)

type MonadInterface m = (MonadError UserThrowableError m, MonadIO m)

runMonadInterface :: Show a => InterfaceM a -> IO ()
runMonadInterface = runInterfaceM 
    >>> runExceptT 
    >=> either print print
