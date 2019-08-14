module Kima.Interface.Types where

import Control.Monad.Except
import Control.Exception

import Data.Void

import Kima.Frontend
import Kima.Interpreter
import Kima.Typechecking
import Text.Megaparsec
import Data.Text.Prettyprint.Doc

class UserThrowable err where
    userShow :: err -> String

    default userShow :: Pretty err => err -> String
    userShow = show . pretty

newtype CustomError = CustomError String
instance UserThrowable CustomError where
    userShow (CustomError str) = str

instance UserThrowable (ParseErrorBundle String Void) where
    userShow = errorBundlePretty

instance UserThrowable TypecheckingError
instance UserThrowable RuntimeError

instance UserThrowable UserThrowableError where
    userShow (UserThrowableError err) = userShow err

data UserThrowableError = forall err. UserThrowable err => UserThrowableError err
instance Show UserThrowableError where
    show (UserThrowableError err) = userShow err

class MonadIO m => MonadInterface m where
    userThrow :: UserThrowable e => e -> m a
instance MonadInterface IO where
    userThrow = throwIO . userError . userShow

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither (Right val) = return val
runEither (Left err) = userThrow err

runMaybe :: (UserThrowable err, MonadInterface m) => err -> Maybe a -> m a
runMaybe _   (Just a) = pure a
runMaybe err Nothing  = userThrow err
