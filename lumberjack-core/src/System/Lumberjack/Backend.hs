{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Generic interface for logging backends.
-- Copyright:    (c) 2015-2018, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Generic interface for logging backends. Inspired by
-- <https://hackage.haskell.org/package/fast-logger fast-logger> package
-- created by Kazu Yamamoto \<kazu@iij.ad.jp\> under
-- <https://github.com/kazu-yamamoto/logger/blob/master/fast-logger/LICENSE BSD3 license>.
module System.Lumberjack.Backend
    (
    -- * Usage Examples

    -- ** Passing Using Monad Transformer
    --
    -- $monadTransformer

    -- ** Passing Using Implicit Parameters
    --
    -- $implicitParameters

    -- ** Passing Using Reflection
    --
    -- $reflection

    -- * Logging Backend Type Class
      LoggingBackend(..)

    -- * Existential Wrapper for Logging Backend
    --
    -- | Code that doesn't require any specific logging backend should be
    -- polymorphic, in most cases this approach is sufficient. In example:
    --
    -- @
    -- doSomething :: 'LoggingBackend' b => b -> IO ()
    -- doSomething loggingBackend = do
    --     -- -->8--
    --     'pushLogStr' loggingBackend \"Some log message.\"
    --     -- -->8--
    --     pure ()
    -- @
    --
    -- This, however, complicates type signatures. In case of things like
    -- passing logging backend around using variosu mechanisms, like
    -- /Implicit Parameters/, /Reflection/, or /Monad Transformer/, it might
    -- make passing logging backend much more painful, or even imposible,
    -- without using monomorphic type. For such cases this library provides
    -- 'SomeLoggingBackend', which is existential wrapper for a
    -- 'LoggingBackend' instance. This hides implementation details and exposes
    -- only 'LoggingBackend' interface.
    --
    -- Modified example:
    --
    -- @
    -- doSomething :: 'SomeLoggingBackend' -> IO ()
    -- doSomething loggingBackend = do
    --     -- -->8--
    --     'pushLogStr' loggingBackend \"Some log message.\"
    --     -- -->8--
    --     pure ()
    -- @
    , SomeLoggingBackend(SomeLoggingBackend)
    , asSomeLoggingBackend
    , asSomeLoggingBackendM
    , withSomeLoggingBackend
    , withSomeLoggingBackendM
    , noLoggingBackend
    )
  where

import Prelude (error)

import Control.Applicative (pure)
import Control.Monad (Monad, (>>=))
import Data.Function (($), flip)
import Data.Typeable (Typeable)
import Data.Void (Void)
import System.IO (IO)

import Data.Default.Class (Default(def))
import Data.LogStr (LogStr)


-- | Describes operations that can be performed on logging backend except its
-- creation, which is best left to specialized smart constructors.
class LoggingBackend b where
    -- | If backend is using files\/network to store\/send log messages, then
    -- by calling this function it should reopen file or network connection.
    --
    -- When log files being rotated it is necessary to reopen it to finalize
    -- the swap of new and old log file. Similarly after detecting network
    -- issues and/or changing configuration at run-time, backend needs to
    -- create new network connection and deallocate the old one.
    reload :: b -> IO ()

    -- | Writing a log message using the specified logging backend.
    pushLogStr :: b -> LogStr -> IO ()

    -- | Flush any log messages in a buffer awaiting to be written\/sent\/etc.
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

    flush (SomeLoggingBackend backend) = flush backend
    {-# INLINE flush #-}

    close (SomeLoggingBackend backend) = close backend
    {-# INLINE close #-}

-- | Interpreted as \"no logging\"; used to implement
-- @'def' :: 'SomeLoggingBackend'@.
instance LoggingBackend Void where
    reload _ = pure ()
    {-# INLINE reload #-}

    pushLogStr   _ _msg = pure ()
    {-# INLINE pushLogStr #-}

    flush _ = pure ()
    {-# INLINE flush #-}

    close _ = pure ()
    {-# INLINE close #-}

-- | Backend implementation for \"no logging\".
--
-- @
-- 'noLoggingBackend' =
--   'SomeLoggingBackend' (error \"SomeLoggingBackend Void\" :: 'Void')
-- @
noLoggingBackend :: SomeLoggingBackend
noLoggingBackend = SomeLoggingBackend (error "SomeLoggingBackend Void" :: Void)
{-# INLINE noLoggingBackend #-}

-- | Please, use 'noLoggingBackend' instead of 'def' whenever possible.
-- 'Default' class has no axioms associated with it, which can make code that
-- uses it hard to reason about.
--
-- @
-- 'def' = 'SomeLoggingBackend' (error \"SomeLoggingBackend Void\" :: 'Void')
-- @
instance Default SomeLoggingBackend where
    def = noLoggingBackend
    {-# INLINE def #-}

-- | Wrap logging backend in to 'SomeLoggingBackend' for it to be used by
-- monomorphic function.
--
-- Usage example:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- -- -->8--
-- import System.Lumberjack.FastLogger (fastLogger)
--
-- -- -->8--
--
-- main :: IO ()
-- main = do
--     config <- parseCommandLineOptions
--     loggingBackend <- fastLogger (config ^. loggerSettings)
--     'withSomeLoggingBackend' loggingBackend $ do
--         -- -->8--
--         'pushLogStr' loggingBackend \"Some message.\"
--         -- -->8--
--         pure ()
-- @
--
-- There is also a flipped version of this function and it is named
-- 'asSomeLoggingBackend'.
withSomeLoggingBackend
    :: LoggingBackend b
    => b
    -> (SomeLoggingBackend -> a)
    -> a
withSomeLoggingBackend backend = ($ SomeLoggingBackend backend)
{-# INLINE withSomeLoggingBackend #-}

-- | Monadic version of 'withSomeLoggingBackendM'.
--
-- Usage example:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- -- -->8--
-- import System.Lumberjack.FastLogger (fastLogger)
--
-- -- -->8--
--
-- main :: IO ()
-- main = do
--     config <- parseCommandLineOptions
--     'withSomeLoggingBackendM' (mkLoggingBackend config) $ \\loggingBackend ->
--         -- -->8--
--         'pushLogStr' loggingBackend \"Some message.\"
--         -- -->8--
--         pure ()
--   where
--     mkLoggingBackend cfg = fastLogger (cfg ^. loggerSettings)
-- @
--
-- There is also a flipped version of this function and it is named
-- 'asSomeLoggingBackendM'.
withSomeLoggingBackendM
    :: (Monad m, LoggingBackend b)
    => m b
    -- ^ Action that creates\/initializes logging backend.
    -> (SomeLoggingBackend -> m a)
    -> m a
withSomeLoggingBackendM = flip asSomeLoggingBackendM
{-# INLINE withSomeLoggingBackendM #-}

-- | Wrap logging backend in to 'SomeLoggingBackend' for it to be used by
-- monomorphic function.
--
-- Usage example:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- -- -->8--
-- import System.Lumberjack.FastLogger (fastLogger)
--
-- -- -->8--
--
-- main' :: SomeLoggingBackend -> IO ()
-- main' = do
--     -- -->8--
--     'pushLogStr' loggingBackend \"Some message.\"
--     -- -->8--
--     pure ()
--
-- main :: IO ()
-- main = do
--     config <- parseCommandLineOptions
--     fastLogger (config ^. loggerSettings) >>= 'asSomeLoggingBackend' main'
-- @
--
-- There is also a flipped version of this function and it is named
-- 'withSomeLoggingBackend'.
asSomeLoggingBackend
    :: LoggingBackend b => (SomeLoggingBackend -> a) -> b -> a
asSomeLoggingBackend = flip withSomeLoggingBackend
{-# INLINE asSomeLoggingBackend #-}

-- | Monadic version of 'asSomeLoggingBackendM'.
--
-- Usage example:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- -- -->8--
-- import System.Lumberjack.FastLogger (fastLogger)
--
-- -- -->8--
--
-- main' :: SomeLoggingBackend -> IO ()
-- main' = do
--     -- -->8--
--     'pushLogStr' loggingBackend \"Some message.\"
--     -- -->8--
--     pure ()
--
-- main :: IO ()
-- main = do
--     config <- parseCommandLineOptions
--     'asSomeLoggingBackendM' main' . fastLogger $ config ^. loggerSettings
-- @
--
-- There is also a flipped version of this function and it is named
-- 'withSomeLoggingBackendM'.
asSomeLoggingBackendM
    :: (Monad m, LoggingBackend b)
    => (SomeLoggingBackend -> m a)
    -> m b
    -- ^ Action that creates\/initializes logging backend.
    -> m a
asSomeLoggingBackendM f = (>>= asSomeLoggingBackend f)
{-# INLINE asSomeLoggingBackendM #-}

-- $monadTransformer
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- module Main (main)
--   where
--
-- import Control.Monad.Logger (LoggingT, pushLogLn, runLoggingT)
-- import Data.Default.Class (Default(def))
--     -- <https://hackage.haskell.org/package/data-default-class>
-- import System.Lumberjack.Backend ('withSomeLoggingBackendM')
-- import System.Lumberjack.FastLogger (fastLogger)
--
--
-- doSomething :: LoggingT IO ()
-- doSomething = do
--     -- -->8--
--     pushLogLn \"Some log message.\"
--     -- -->8--
--     pure ()
--
-- main :: IO ()
-- main = 'withSomeLoggingBackendM' (fastLogger def) $ runLoggingT doSomething
-- @

-- $implicitParameters
--
-- While `ImplicitParams` language extension makes it easy to pass logging
-- effect around, it also makes the code less predictable in some ways. Try to
-- avoid using this unless you have a good reason to use this extension.
--
-- @
-- {-\# LANGUAGE ImplicitParams \#-}
-- {-\# LANGUAGE OverloadedStrings \#-}
-- module Main (main)
--   where
--
-- import Data.Default.Class (Default(def))
--     -- <https://hackage.haskell.org/package/data-default-class>
-- import System.Lumberjack.Backend
--     ( 'LoggingBackend'('pushLogStr')
--     , 'SomeLoggingBackend'
--     , 'withSomeLoggingBackendM'
--     )
-- import System.Lumberjack.FastLogger (fastLogger)
--
--
-- doSomething :: (?loggingBackend :: 'SomeLoggingBackend') => IO ()
-- doSomething = do
--     -- -->8--
--     'pushLogStr' ?loggingBackend \"Some log message.\"
--     -- -->8--
--     pure ()
--
-- main :: IO ()
-- main = 'withSomeLoggingBackendM' (fastLogger def) $ \\loggingBackend ->
--     let ?loggingBackend = loggingBackend in doSomething
-- @
--
-- Form more information see Haskell Wiki article
-- <https://wiki.haskell.org/Implicit_parameters Implicit parameters>.

-- $reflection
--
-- @
-- {-\# LANGUAGE FlexibleContexts \#-}
-- {-\# LANGUAGE OverloadedStrings \#-}
-- {-\# LANGUAGE ScopedTypeVariables \#-}
-- module Main (main)
--   where
--
-- import Data.Proxy (Proxy(Proxy))
--
-- import Data.Default.Class (Default(def))
--     -- <https://hackage.haskell.org/package/data-default-class>
-- import Data.Functor.Trans.Tagged (TaggedT(TagT), proxyT)
--     -- <https://hackage.haskell.org/package/tagged-transformer>
-- import Data.Reflection (Reifies(reflect), reify)
--     -- <https://hackage.haskell.org/package/reflection>
-- import System.Lumberjack.Backend
--     ( 'LoggingBackend'('pushLogStr')
--     , 'SomeLoggingBackend'
--     , 'withSomeLoggingBackendM'
--     )
-- import System.Lumberjack.FastLogger (fastLogger)
--
--
-- doSomething :: forall s. Reifies s 'SomeLoggingBackend' => TaggedT s IO ()
-- doSomething = TagT $ do
--     -- -->8--
--     'pushLogStr' loggingBackend \"Some log message.\"
--     -- -->8--
--     pure ()
--   where
--     loggingBackend = reflect (Proxy :: Proxy s)
--
-- main :: IO ()
-- main = 'withSomeLoggingBackendM' (fastLogger def) $ \loggingBackend ->
--     reify loggingBackend (proxyT doSomething)
-- @
--
-- For more information on /Scoped Type Variables/ language extension see
-- Haskell Wiki article
-- <https://wiki.haskell.org/Scoped_type_variables Scoped type variables>.
--
-- Understanding /Reflection/ is little more complicated. There is a great
-- article on School of Haskell
-- <https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection Reflecting values to types and back>.
