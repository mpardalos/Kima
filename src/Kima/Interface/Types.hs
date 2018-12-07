module Kima.Interface.Types where

import Control.Arrow hiding (first)
import Control.Monad.Except
import Control.Exception

import Kima.Frontend
import Kima.Interpreter

class UserThrowable err where
    userShow :: err -> String

    default userShow :: Show err => err -> String
    userShow = show

newtype CustomError = CustomError String
instance UserThrowable CustomError where
    userShow (CustomError str) = str

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

runMonadInterface :: InterfaceM a -> IO a
runMonadInterface = runInterfaceM 
    >>> runExceptT 
    >=> \case 
        Left err -> throwIO $ userError (show err)
        Right a -> pure a
