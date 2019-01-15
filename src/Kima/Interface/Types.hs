module Kima.Interface.Types where

import Control.Arrow hiding (first)
import Control.Monad.Except
import Control.Exception

import Data.Void
import Data.Bifunctor

import Kima.Frontend
import Kima.Interpreter
import Kima.Typechecking
import Text.Megaparsec

class UserThrowable err where
    userShow :: err -> String

    default userShow :: Show err => err -> String
    userShow = show

newtype CustomError = CustomError String
instance UserThrowable CustomError where
    userShow (CustomError str) = str

instance UserThrowable (ParseErrorBundle String Void) where
    userShow = errorBundlePretty

instance UserThrowable TypecheckingError
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

userThrow :: (MonadInterface m, UserThrowable err) => err -> m a
userThrow = throwError . UserThrowableError

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither = liftEither . bimap UserThrowableError id 

runMaybe :: (UserThrowable err, MonadInterface m) => err -> Maybe a -> m a
runMaybe _   (Just a) = pure a 
runMaybe err Nothing  = userThrow err