{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       $HEADER$
-- Description:  FastLogger backend for Lumberjack logging framework.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude, RecordWildCards
--
-- 'FastLogger' backend for Lumberjack logging framework.
module System.Lumberjack.FastLogger
    (
    -- * Fast Logger
      FastLogger

    -- ** Construct Fast Logger
    , fastLogger
    , FastLoggerSettings
    , BufSize
    , LoggingOutput(..)
    , bufferSize
    , loggingOutput
    )
  where

import Data.Functor ((<$>))
import System.IO (IO)

import System.Log.FastLogger (BufSize)
import qualified System.Log.FastLogger as FastLogger
    ( newFileLoggerSet
    , newStderrLoggerSet
    , newStdoutLoggerSet
    )

import System.Lumberjack.FastLogger.Internal
    ( FastLogger(FastLogger)
    , FastLoggerSettings(FastLoggerSettings, _bufferSize, _loggingOutput)
    , LoggingOutput(LogFile, LogStderr, LogStdout)
    , bufferSize
    , loggingOutput
    )


-- | Construct a 'FastLogger' instance using settings provided in
-- 'FastLoggerSettings'. By default it writes log messages in to /stderr/ and
-- buffer size is set to 'System.Log.FastLogger.defaultBufSize'.
fastLogger :: FastLoggerSettings -> IO FastLogger
fastLogger FastLoggerSettings{..} = FastLogger <$> case _loggingOutput of
    LogFile filePath -> FastLogger.newFileLoggerSet   _bufferSize filePath
    LogStderr        -> FastLogger.newStderrLoggerSet _bufferSize
    LogStdout        -> FastLogger.newStdoutLoggerSet _bufferSize
{-# INLINE fastLogger #-}
