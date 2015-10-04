{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Internal definitions that may come handy when writing a
--               library.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NamedFieldPuns,
--               NoImplicitPrelude
--
-- Internal definitions that may come handy when writing a library.
module System.Lumberjack.FastLogger.Internal
  where

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (Functor, (<$>))
import GHC.Generics (Generic)
import System.IO (FilePath)
import Unsafe.Coerce (unsafeCoerce)

import Data.Default.Class (Default(def))
import System.Log.FastLogger (BufSize, LoggerSet)
import qualified System.Log.FastLogger as FastLogger
    ( defaultBufSize
    , flushLogStr
    , pushLogStr
    , pushLogStrLn
    , renewLoggerSet
    , rmLoggerSet
    )
import System.Lumberjack.Backend
    ( LoggingBackend
        ( close
        , flush
        , pushLogStr
        , pushLogStrLn
        , reload
        )
    )


-- | Log messages can be written in to destinations defined in this data type.
data LoggingOutput
    = LogFile !FilePath
    -- ^ Write log messages in to specified /file/. See also
    -- 'System.Lumberjack.FastLogger.newFileLoggerSet'.
    | LogStderr
    -- ^ Write log messages in to /stderr/. See also
    -- 'System.Lumberjack.FastLogger.newStdoutLoggerSet'.
    | LogStdout
    -- ^ Write log messages in to /stdout/. See also
    -- 'System.Lumberjack.FastLogger.newStderrLoggerSet'.
  deriving (Data, Eq, Generic, Typeable)

-- | Settings for creating 'FastLogger' instance. Use 'bufferSize' and
-- 'loggingOutput' lenses to modify default value.
--
-- @
-- import Control.Lens ((.~), (&))
-- import System.Lumberjack.FastLogger
--
-- myFastLoggerSettings :: FilePath -> 'FastLoggerSettings'
-- myFastLoggerSettings logFile = 'def'
--     & 'bufferSize' .~ 32768 -- 32k
--     & 'loggingOutput' .~ 'LogFile' logFile
-- @
data FastLoggerSettings = FastLoggerSettings
    { _bufferSize :: !BufSize
    -- ^ See 'bufferSize' for details.
    , _loggingOutput :: !LoggingOutput
    -- ^ See 'loggingOutput' for details.
    }
  deriving (Data, Eq, Generic, Typeable)

-- | FastLogger creates a message buffer for each of its threads, size of those
-- buffers can be configured using this lens. 'FastLoggerSettings' use default
-- value from Fast Logger library 'FastLogger.defaultBufSize'.
bufferSize
    :: Functor f
    => (BufSize -> f BufSize)
    -> FastLoggerSettings
    -> f FastLoggerSettings
bufferSize f s@FastLoggerSettings{_bufferSize} =
    (\b -> s{_bufferSize = b}) <$> f _bufferSize
{-# INLINE bufferSize #-}

-- | Log messages can be written in to various destinations. Fast Logger allows
-- three types of destination /stderr/, /stdout/, and /file/. See
-- 'LoggingOutput' for details.
loggingOutput
    :: Functor f
    => (LoggingOutput -> f LoggingOutput)
    -> FastLoggerSettings
    -> f FastLoggerSettings
loggingOutput f s@FastLoggerSettings{_loggingOutput} =
    (\b -> s{_loggingOutput = b}) <$> f _loggingOutput
{-# INLINE loggingOutput #-}

-- | @
-- 'bufferSize' = 'FastLogger.defaultBufSize'
-- 'loggingOutput' = 'LogStderr'
-- @
instance Default FastLoggerSettings where
    def = FastLoggerSettings
        { _bufferSize = FastLogger.defaultBufSize
        , _loggingOutput = LogStderr
        }
    {-# INLINE def #-}

-- | Wrapper for 'LoggerSet' to avoid orphan instances and increase type
-- safety. Later comes handy in case when some parts of code base use Fast
-- Logger directly and some don't.
newtype FastLogger = FastLogger LoggerSet
  deriving (Generic, Typeable)

instance LoggingBackend FastLogger where
    reload (FastLogger loggerSet) = FastLogger.renewLoggerSet loggerSet
    {-# INLINE reload #-}

    pushLogStrLn (FastLogger loggerSet) =
        FastLogger.pushLogStrLn loggerSet . unsafeCoerce
        -- Package fast-logger doesn't expose LogStr constructor. Using
        -- unsafeCoerce is correct in this example since LogStr defined in
        -- Lumberjack is identical to the one defined in fast-logger.
    {-# INLINE pushLogStrLn #-}

    pushLogStr (FastLogger loggerSet) =
        FastLogger.pushLogStr loggerSet . unsafeCoerce
        -- Package fast-logger doesn't expose LogStr constructor. Using
        -- unsafeCoerce is correct in this example since LogStr defined in
        -- Lumberjack is identical to the one defined in fast-logger.
    {-# INLINE pushLogStr #-}

    flush (FastLogger loggerSet) = FastLogger.flushLogStr loggerSet
    {-# INLINE flush #-}

    close (FastLogger loggerSet) = FastLogger.rmLoggerSet loggerSet
    {-# INLINE close #-}
