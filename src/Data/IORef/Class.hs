module Data.IORef.Class (
    MonadIORef(..),
    IORef
) where

import           Data.IORef (IORef)
import qualified Data.IORef as IORefOrig
import           Control.Monad.IO.Class

-- | Monads that can interact with IORefs.
-- | Basically limited to IO and anything implementing MonadIO
-- | The default instance uses the IORef functions from base using liftIO
class Monad m => MonadIORef m where
    newIORef :: a -> m (IORef a)
    readIORef :: IORef a -> m a
    writeIORef :: IORef a -> a -> m ()
    modifyIORef :: IORef a -> (a -> a) -> m ()

    default readIORef :: MonadIO m => IORef a -> m a
    readIORef ref = liftIO (IORefOrig.readIORef ref)

    default writeIORef :: MonadIO m => IORef a -> a -> m ()
    writeIORef ref val = liftIO (IORefOrig.writeIORef ref val)

    default modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
    modifyIORef ref f = liftIO (IORefOrig.modifyIORef ref f)

    default newIORef :: MonadIO m => a -> m (IORef a)
    newIORef x = liftIO (IORefOrig.newIORef x)


instance MonadIORef IO where
