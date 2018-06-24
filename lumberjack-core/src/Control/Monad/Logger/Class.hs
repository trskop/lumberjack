{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitForAll #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2018, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extension.
--
-- TODO
module Control.Monad.Logger.Class
  where

#ifndef APPLICATIVE_MONAD
import Control.Applicative (Applicative)
#endif
import Control.Monad (Monad)
import Data.Function ((.))
import System.IO (IO)

import Control.Monad.IO.Class (MonadIO)

import Data.LogStr (LogStr, LogStrArgs(logStrArgs))

import System.Lumberjack.Backend (SomeLoggingBackend)
import System.Lumberjack.PushLog (PushLog)


-- | Instances of this type class declare that they have the ability to send
-- log message to a logging backend.
class
    ( Monad m
#ifndef APPLICATIVE_MONAD
    , Applicative m
#endif
    )
    => MonadLogger m
  where
    -- | Evaluates 'PushLog' closure by providing access to logging backend.
    -- This function is alwaysed used with some kind of smart constructor of
    -- 'PushLog', since it doesn't directly mention any logging message.
    --
    -- See also 'runPushLog', and 'pushLogStr'.
    runPushLog :: PushLog SomeLoggingBackend -> m ()

-- | Monads that are instances of this type class have access to raw unlifted
-- 'runPushLog'. Such monad has to be 'MonadIO' instance, since raw unlifted
-- 'runPushLog' has 'IO' side effect, and without that 'MonadIO' instance it
-- wouldn't be able to use it.
class (MonadLogger m, MonadIO m) => MonadLoggerIO m where
    askRunPushLog :: m (PushLog SomeLoggingBackend -> IO ())

-- | Send 'LogStr' to logging backend without any changes.
pushLogStr :: MonadLogger m => LogStr -> m ()
pushLogStr = runPushLog . logStrArgs
