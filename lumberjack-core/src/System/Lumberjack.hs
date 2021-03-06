{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2018, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module System.Lumberjack
    (
    -- * Logging Backend
      LoggingBackend(..)

    -- ** Existential Wrapper for Logging Backend
    , SomeLoggingBackend(..)
    , asSomeLoggingBackend
    , withSomeLoggingBackend

    -- * Log Message
    --
    -- | Log Messages are represented by 'LogStr' data type that behaves like
    -- builder with additional O(1) length operation. This is important to
    -- provide effective message buffering.
    , LogStr
    , fromLogStr

    -- ** Conversion To LogStr
    , ToLogStr(toLogStr)

    -- *** Hexadecimal Representation
    --
    -- | Usage example:
    --
    -- >>> fromLogStr . toLogStr $ hex (123 :: Int64)
    -- "000000000000007b"
    , hex

    -- *** Conversion Using Show Instances
    , showed
    , showed1
    , showed2

    -- * Generic Logging Function
    --
    -- | Usage example:
    --
    -- >>> fromLogStr $ logStr "Important variable: " (123 :: Int)
    -- "Important variable: 123"
    , LogStrArgs(Result, logStrArgs)
    , logStr

    -- * Location
    , Location

    -- * Logging Levels
--  , LogLevel(..)

    -- * PushLog Closure
    , PushLog
    , mkPushLog

    -- ** Run PushLog
    , pushLog
    )
  where

--import Data.LogLevel
--    ( LogLevel
--        ( LevelAlert
--        , LevelCritical
--        , LevelDebug
--        , LevelEmergency
--        , LevelError
--        , LevelInfo
--        , LevelNotice
--        , LevelOther
--        , LevelWarning
--        )
--     )
import Data.LogStr
    ( LogStr
    , LogStrArgs(Result, logStrArgs)
    , ToLogStr(toLogStr)
    , fromLogStr
    , hex
    , logStr
    , showed
    , showed1
    , showed2
    )

import System.Lumberjack.Backend
    ( LoggingBackend
        ( close
        , flush
        , pushLogStr
        , reload
        )
    , SomeLoggingBackend(SomeLoggingBackend)
    , asSomeLoggingBackend
    , withSomeLoggingBackend
    )
import System.Lumberjack.Location (Location)
import System.Lumberjack.PushLog
    ( PushLog
    , mkPushLog
    , pushLog
    )
