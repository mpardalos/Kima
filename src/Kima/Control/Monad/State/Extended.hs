module Kima.Control.Monad.State.Extended (
    module Control.Monad.State,
    withState
) where

import Control.Monad.State hiding (withState)

withState :: MonadState s m => (s -> s) -> m a -> m a
withState f a = do
    originalState <- get
    modify f *> a <* modify (const originalState)


