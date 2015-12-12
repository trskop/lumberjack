{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               NoImplicitPrelude, RankNTypes, TypeFamilies
--
-- TODO
module System.Lumberjack.PushLog
  where

import Control.Monad (Monad((>>), return))
import Data.Function ((.), ($), const, flip)
import Data.Typeable (Typeable)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import System.IO (IO)

import Data.Default.Class (Default(def))

import System.Lumberjack.Backend (LoggingBackend(..))
import System.Lumberjack.LogStr (LogArgs(..), LogStr)


-- | Represents closure of a function like 'pushLogStr' or 'pushLogStrLn', with
-- already bounded 'LogStr' argument.
newtype PushLog b t = PushLog (b -> IO ())
  deriving (Generic, Typeable)

noop :: PushLog b t
noop = PushLog . const $ return ()

-- | Doesn't push a log message in to backend, only returns ().
instance Default (PushLog b t) where
    def = noop

instance Monoid (PushLog b t) where
    mempty = noop
    PushLog f `mappend` PushLog g =
        PushLog $ \backend -> f backend >> g backend

-- | Run 'PushLog' using provided logging backend.
runPushLog :: LoggingBackend b => b -> PushLog b t -> IO ()
runPushLog backend (PushLog f) = f backend
{-# INLINE runPushLog #-}

-- | Variant of 'runPushLog' where type variable @t@ in @'PushLog' b t@ can is
-- restricted by type proxy. Implemented as:
--
-- @
-- 'runPushLogTaggedWith' :: 'Proxy' t -> b -> 'PushLog' b t -> IO ()
-- 'runPushLogTaggedWith' 'Proxy' = 'runPushLog'
-- @
runPushLogTaggedWith
    :: LoggingBackend b
    => Proxy t
    -> b
    -> PushLog b t
    -> IO ()
runPushLogTaggedWith Proxy = runPushLog
{-# INLINE runPushLogTaggedWith #-}

-- | Smart constructor for 'PushLog'.
mkPushLog
    :: LoggingBackend b
    => (b -> LogStr -> IO ())
    -> LogStr
    -> PushLog b t
mkPushLog push = PushLog . flip push
{-# INLINE mkPushLog #-}

forgetPushLogTag
    :: LoggingBackend b
    => PushLog b t
    -> forall t'. PushLog b t'
forgetPushLogTag (PushLog f) = PushLog f
{-# INLINE forgetPushLogTag #-}

data Str
  deriving (Generic, Typeable)

str :: Proxy Str
str = Proxy

data Line
  deriving (Generic, Typeable)

line :: Proxy Line
line = Proxy

-- | Run 'PushLog' using provided logging backend without appending new line at
-- the end of the log message.
pushLog :: LoggingBackend b => b -> PushLog b Str -> IO ()
pushLog = runPushLogTaggedWith str
{-# INLINE pushLog #-}

-- | Run 'PushLog' using provided logging backend with new line appended to the
-- end of the log message.
pushLogLn :: LoggingBackend b => b -> PushLog b Line -> IO ()
pushLogLn = runPushLogTaggedWith line
{-# INLINE pushLogLn #-}

instance LoggingBackend b => LogArgs (PushLog b Str) where
    type Result (PushLog b Str) = PushLog b Str

    logArgs = mkPushLog pushLogStr
    {-# INLINE logArgs #-}

instance LoggingBackend b => LogArgs (PushLog b Line) where
    type Result (PushLog b Line) = PushLog b Line

    logArgs = mkPushLog pushLogStrLn
    {-# INLINE logArgs #-}
