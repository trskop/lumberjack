{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
module System.Lumberjack.PushLog
    ( PushLog(..)
    , mkPushLog
    , noop
    , runPushLog
    , pushLog
    )
  where

import Control.Applicative ((*>), pure)
import Data.Function ((.), ($), const, flip)
import Data.Typeable (Typeable)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Semigroup (Semigroup((<>)))
import GHC.Generics (Generic)
import System.IO (IO)

import Data.Default.Class (Default(def))

import System.Lumberjack.Backend (LoggingBackend, pushLogStr)
import Data.LogStr (LogStrArgs(Result, logStrArgs), LogStr)


-- | Represents closure of a function like 'pushLogStr' or 'pushLogStrLn', with
-- already bounded 'LogStr' argument.
newtype PushLog b = PushLog (b -> IO ())
  deriving (Generic, Typeable)

noop :: PushLog b
noop = PushLog . const $ pure ()
{-# INLINE noop #-}

append :: PushLog b -> PushLog b -> PushLog b
append (PushLog f) (PushLog g) =
    PushLog $ \backend -> f backend *> g backend
{-# INLINE append #-}

-- | Doesn't push a log message in to backend, only returns ().
instance Default (PushLog b) where
    def = noop
    {-# INLINE def #-}

instance Semigroup (PushLog b) where
    (<>) = append
    {-# INLINE (<>) #-}

instance Monoid (PushLog b) where
    mempty = noop
    {-# INLINE mempty #-}

    mappend = append
    {-# INLINE mappend #-}

-- | Run 'PushLog' using provided logging backend.
runPushLog :: LoggingBackend b => b -> PushLog b -> IO ()
runPushLog backend (PushLog f) = f backend
{-# INLINE runPushLog #-}

-- | Smart constructor for 'PushLog'.
mkPushLog
    :: LoggingBackend b
    => (b -> LogStr -> IO ())
    -> LogStr
    -> PushLog b
mkPushLog push = PushLog . flip push
{-# INLINE mkPushLog #-}

-- | Run 'PushLog' using provided logging backend without appending new line at
-- the end of the log message.
pushLog :: LoggingBackend b => b -> PushLog b -> IO ()
pushLog = runPushLog
{-# INLINE pushLog #-}

instance LoggingBackend b => LogStrArgs (PushLog b) where
    type Result (PushLog b) = PushLog b

    logStrArgs = mkPushLog pushLogStr
    {-# INLINE logStrArgs #-}
