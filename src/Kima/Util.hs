module Kima.Util where

import System.IO.Unsafe
import OpenTelemetry.Eventlog

withSpanM_ :: Monad m => String -> m a -> m a
withSpanM_ name action = do
    let !s = unsafePerformIO (beginSpan name)
    res <- action
    let !_ = unsafePerformIO (endSpan s)
    return res
