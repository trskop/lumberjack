{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               NoImplicitPrelude, TypeFamilies
--
-- TODO
module System.Lumberjack.PushLog
  where

import Control.Monad (Monad((>>), return))
import Data.Function ((.), ($), const, flip)
import Data.Typeable (Typeable)
import Data.Monoid (Monoid(mappend, mempty))
import GHC.Generics (Generic)
import System.IO (IO)

import Data.Default.Class (Default(def))

import System.Lumberjack.Backend (LoggingBackend(..))
import System.Lumberjack.LogStr (LogStrArgs(..), LogStr)


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

-- | Smart constructor for 'PushLog'.
mkPushLog
    :: LoggingBackend b
    => (b -> LogStr -> IO ())
    -> LogStr
    -> PushLog b t
mkPushLog push = PushLog . flip push
{-# INLINE mkPushLog #-}

data Str
  deriving (Generic, Typeable)

data Line
  deriving (Generic, Typeable)

-- | Run 'PushLog' using provided logging backend without appending new line at
-- the end of the log message.
pushLog :: LoggingBackend b => b -> PushLog b Str -> IO ()
pushLog = runPushLog
{-# INLINE pushLog #-}

-- | Run 'PushLog' using provided logging backend with new line appended to the
-- end of the log message.
pushLogLn :: LoggingBackend b => b -> PushLog b Line -> IO ()
pushLogLn = runPushLog
{-# INLINE pushLogLn #-}

instance LoggingBackend b => LogStrArgs (PushLog b Str) where
    type Result (PushLog b Str) = PushLog b Str

    logStrArgs = mkPushLog pushLogStr
    {-# INLINE logStrArgs #-}

instance LoggingBackend b => LogStrArgs (PushLog b Line) where
    type Result (PushLog b Line) = PushLog b Line

    logStrArgs = mkPushLog pushLogStrLn
    {-# INLINE logStrArgs #-}
