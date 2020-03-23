module Kima.Interface.Monad where

import System.Exit
import System.IO

import Control.Monad.Except

import Data.Void

import Kima.Syntax
import Kima.Interpreter
import Kima.Types
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

instance UserThrowable TypecheckingError where
    userShow err = "Type Error: " <> show (pretty err) -- 

instance UserThrowable RuntimeError where
    userShow err = "Runtime Error: " <> show (pretty err)

instance UserThrowable UserThrowableError where
    userShow (UserThrowableError err) = userShow err

data UserThrowableError = forall err. UserThrowable err => UserThrowableError err
instance Show UserThrowableError where
    show (UserThrowableError err) = userShow err

newtype UserInterface a = UserInterface { runUserInterface :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)
class MonadIO m => MonadInterface m where
    userThrow :: UserThrowable e => e -> m a

instance MonadInterface UserInterface where
    userThrow err = liftIO $ do
        hPutStrLn stderr (userShow err)
        exitFailure -- TODO throw an exception and catch it in main

runEither :: (MonadInterface m, UserThrowable err) => Either err a -> m a
runEither (Right val) = return val
runEither (Left err) = userThrow err

runMaybe :: (UserThrowable err, MonadInterface m) => err -> Maybe a -> m a
runMaybe _   (Just a) = pure a
runMaybe err Nothing  = userThrow err
