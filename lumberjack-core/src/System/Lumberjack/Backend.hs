{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, ExistentialQuantification,
--               NoImplicitPrelude
--
-- Inspired by:
-- <https://hackage.haskell.org/package/fast-logger fast-logger> created by
-- Kazu Yamamoto \<kazu@iij.ad.jp\> under
-- <https://github.com/kazu-yamamoto/logger/blob/master/fast-logger/LICENSE BSD3 license>.
module System.Lumberjack.Backend
    (
    -- * Usage Examples

    -- ** Passing Backend Using Implicit Parameters
    --
    -- $implicitParameters

    -- * Logging Backend Type Class
      LoggingBackend(..)

    -- * Existential Wrapper for Logging Backend
    , SomeLoggingBackend(SomeLoggingBackend)
    , asSomeLoggingBackend
    , asSomeLoggingBackendM
    , withSomeLoggingBackend
    , withSomeLoggingBackendM
    )
  where

import Control.Monad (Monad((>>=)))
import Data.Typeable (Typeable)
import Data.Function (($), flip)
import System.IO (IO)

import Data.LogStr (LogStr)


-- | Describes operations that can be performed on logging backend except its
-- creation, which is best left to specialized smart constructors.
class LoggingBackend b where
    -- | If backend is using files/network to store/send log messages, then by
    -- calling this function it should reopen file or network connection.
    --
    -- When log files being rotated it is necessary to reopen it to finalize
    -- the swap of new and old log file. Similarly after detecting network
    -- issues and/or changing configuration at run-time, backend needs to
    -- create new network connection and deallocate the old one.
    reload :: b -> IO ()

    -- | Writing a log message using the specified logging backend.
    pushLogStr :: b -> LogStr -> IO ()

    -- | Same as 'pushLogStr', but newline is appended to the log message.
    pushLogStrLn :: b -> LogStr -> IO ()

    -- | Flush any log messages in a buffer awaiting to be written/sent/etc.
    flush :: b -> IO ()

    -- | Perform 'flush' and release any acquired resources. Logging backend
    -- may not be used after this step.
    close :: b -> IO ()

-- | Useful in situations when monomorphic interface is desired, but without
-- the loss of generality.
data SomeLoggingBackend = forall b. LoggingBackend b => SomeLoggingBackend b
  deriving (Typeable)

instance LoggingBackend SomeLoggingBackend where
    reload (SomeLoggingBackend backend) = reload backend
    {-# INLINE reload #-}

    pushLogStr (SomeLoggingBackend backend) = pushLogStr backend
    {-# INLINE pushLogStr #-}

    pushLogStrLn (SomeLoggingBackend backend) = pushLogStrLn backend
    {-# INLINE pushLogStrLn #-}

    flush (SomeLoggingBackend backend) = flush backend
    {-# INLINE flush #-}

    close (SomeLoggingBackend backend) = close backend
    {-# INLINE close #-}

-- | Wrap logginb backend in to 'SomeLoggingBackend' for it to be used by
-- monomorphic function.
withSomeLoggingBackend
    :: LoggingBackend b => b -> (SomeLoggingBackend -> a) -> a
withSomeLoggingBackend backend = ($ SomeLoggingBackend backend)
{-# INLINE withSomeLoggingBackend #-}

-- | Monadic version of 'withSomeLoggingBackendM'.
--
-- Usage example:
--
-- @
-- 'withSomeLoggingBackendM' createLoggingBackend $ \\loggingBackend ->
--     -- -->8--
--     'pushLogStrLn' loggingBackend \"Some message.\"
--     -- -->8--
-- @
withSomeLoggingBackendM
    :: (Monad m, LoggingBackend b)
    => m b
    -- ^ Action that creates\/initializes logging backend.
    -> (SomeLoggingBackend -> m a)
    -> m a
withSomeLoggingBackendM = flip asSomeLoggingBackendM
{-# INLINE withSomeLoggingBackendM #-}

-- | Flipped version of 'withSomeLoggingBackend'.
asSomeLoggingBackend
    :: LoggingBackend b => (SomeLoggingBackend -> a) -> b -> a
asSomeLoggingBackend = flip withSomeLoggingBackend
{-# INLINE asSomeLoggingBackend #-}

-- | Flipped version of 'withSomeLoggingBackendM'.
asSomeLoggingBackendM
    :: (Monad m, LoggingBackend b)
    => (SomeLoggingBackend -> m a)
    -> m b
    -- ^ Action that creates\/initializes logging backend.
    -> m a
asSomeLoggingBackendM f = (>>= asSomeLoggingBackend f)
{-# INLINE asSomeLoggingBackendM #-}

-- $implicitParameters
--
-- @
-- {-\# LANGUAGE ImplicitParams \#-}
-- {-\# LANGUAGE OverloadedStrings \#-}
-- module Main (main)
--   where
--
-- import Data.Default.Class (Default(def))
-- import System.Lumberjack.Backend
--     ( 'LoggingBackend'('pushLogStrLn')
--     , 'SomeLoggingBackend'
--     , 'withSomeLoggingBackendM'
--     )
-- import System.Lumberjack.FastLogger (fastLogger)
--
--
-- doSomething :: (?loggingBackend :: 'SomeLoggingBackend') => IO ()
-- doSomething = do
--     -- -->8--
--     'pushLogStrLn' ?loggingBackend \"Some log message.\"
--     -- -->8--
--     return ()
--
-- main :: IO ()
-- main = 'withSomeLoggingBackendM' (fastLogger def) $ \\loggingBackend ->
--     let ?loggingBackend = loggingBackend in doSomething
-- @
