{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitForAll #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP, ExplicitForAll, NoImplicitPrelude
--
-- TODO
module Control.Monad.Logger.Class
  where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Data.Function ((.))
import System.IO (IO)

import Control.Monad.IO.Class (MonadIO)

import System.Lumberjack.Backend (SomeLoggingBackend)
import System.Lumberjack.PushLog (PushLog, line, str)
import System.Lumberjack.LogStr (LogStr, LogArgs(logArgs))


class
#ifdef APPLICATIVE_MONAD
    Monad m
#else
    (Applicative m, Monad m)
#endif
    => MonadLogger m
  where
    {-# MINIMAL runPushLog #-}
    runPushLog :: forall t. PushLog SomeLoggingBackend t -> m ()

    runPushLogTaggedWith
        :: forall proxy t. proxy t -> PushLog SomeLoggingBackend t -> m ()
    runPushLogTaggedWith _ = runPushLog

    logStr :: LogStr -> m ()
    logStr = runPushLogTaggedWith str . logArgs

    logLn :: LogStr -> m ()
    logLn = runPushLogTaggedWith line . logArgs

class (MonadLogger m, MonadIO m) => MonadLoggerIO m where
    askRunPushLog :: forall t. m (PushLog SomeLoggingBackend t -> IO ())
