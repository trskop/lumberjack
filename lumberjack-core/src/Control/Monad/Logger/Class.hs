{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitForAll #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP, ExplicitForAll, NoImplicitPrelude
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
import System.Lumberjack.PushLog (PushLog, line, str)


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
    -- See also 'runPushLogTaggedWith', 'pushLogStr', and 'pushLogLn'.
    runPushLog :: forall t. PushLog SomeLoggingBackend t -> m ()

-- | Monads that are instances of this type class have access to raw unlifted
-- 'runPushLog'. Such monad has to be 'MonadIO' instance, since raw unlifted
-- 'runPushLog' has 'IO' side effect, and without that 'MonadIO' instance it
-- wouldn't be able to use it.
class (MonadLogger m, MonadIO m) => MonadLoggerIO m where
    askRunPushLog :: forall t. m (PushLog SomeLoggingBackend t -> IO ())

-- | Version of 'runPushLog' that restricts type of type tag @t@. Useful in
-- conjunction with 'logStrArgs'.
--
-- In example 'pushLogStr' is defined using 'runPushLogTaggedWith':
--
-- @
-- 'runPushLogTaggedWith' 'str' . 'logStrArgs'
--     :: 'MonadLogger' m => 'LogStr' -> m ()
-- @
runPushLogTaggedWith
    :: forall m proxy t
    . MonadLogger m
    => proxy t
    -> PushLog SomeLoggingBackend t
    -> m ()
runPushLogTaggedWith _ = runPushLog

-- | Send 'LogStr' to logging backend without any changes.
pushLogStr :: MonadLogger m => LogStr -> m ()
pushLogStr = runPushLogTaggedWith str . logStrArgs

-- | Send 'LogStr' to logging backend with new line appended.
pushLogLn :: MonadLogger m => LogStr -> m ()
pushLogLn = runPushLogTaggedWith line . logStrArgs
