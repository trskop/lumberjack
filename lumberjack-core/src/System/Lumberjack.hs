{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- TODO
module System.Lumberjack
    (
    -- * Logging Backend
      LoggingBackend(..)
    , SomeLoggingBackend(..)

    -- * Log Message
    --
    -- | Log Messages are represented by 'LogStr' data type that behaves like
    -- builder with 'logStrLength' O(1) operation. This is important to provide
    -- effective message buffering.
    , LogStr
    , fromLogStr
    , logStrLength

    -- ** Conversion To LogStr
    , ToLogStr(toLogStr)

    -- *** Hexadecimal Representation
    --
    -- | Usage example:
    --
    -- >>> fromLogStr . toLogStr $ hex (123 :: Int64)
    -- "000000000000007b"
    , hex

    -- ** Simplified Concatenation
    --
    -- | Usage example:
    --
    -- >>> fromLogStr $ log "Important variable: " (123 :: Int)
    -- "Important variable: 123"
    , LogArgs(Result, logArgs)
    , log

    -- * Location
    , Location

    -- * Logging Levels
    , LogLevel(..)

    -- * PushLog Closure
    , PushLog
    , mkPushLog

    -- ** Run PushLog
    , pushLog
    , pushLogLn
    )
  where

import System.Lumberjack.Backend
    ( LoggingBackend
        ( close
        , flush
        , pushLogStr
        , pushLogStrLn
        , reload
        )
    , SomeLoggingBackend(SomeLoggingBackend)
    )
import System.Lumberjack.Location (Location)
import System.Lumberjack.LogLevel
    ( LogLevel
        ( LevelAlert
        , LevelCritical
        , LevelDebug
        , LevelEmergency
        , LevelError
        , LevelInfo
        , LevelNotice
        , LevelOther
        , LevelWarning
        )
     )
import System.Lumberjack.LogStr
    ( LogStr
    , LogArgs(Result, logArgs)
    , ToLogStr(toLogStr)
    , fromLogStr
    , hex
    , log
    , logStrLength
    )
import System.Lumberjack.PushLog
    ( PushLog
    , mkPushLog
    , pushLog
    , pushLogLn
    )
