{-# LANGUAGE CPP #-}
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
import Data.Monoid
    ( Monoid
        ( mempty
#ifndef SEMIGROUP_MONOID
        , mappend
#endif
        )
    )
import Data.Semigroup (Semigroup((<>)))
import GHC.Generics (Generic)
import System.IO (IO)

#ifdef WITH_data_default_class
import Data.Default.Class (Default(def))
#endif

import System.Lumberjack.Backend (LoggingBackend, pushLogStr)
import Data.LogStr (LogStrArgs(Result, logStrArgs), LogStr)


-- | Represents closure of a function like 'pushLogStr' or 'pushLogStrLn', with
-- already bounded either 'LogStr' or 'LoggingBackend' argument.
newtype PushLog a = PushLog (a -> IO ())
  deriving (Generic, Typeable)

-- | Doesn't push a log message in to backend, only returns '()'.  Behaves as
-- an identity for 'append' operation.
noop :: PushLog a
noop = PushLog . const $ pure ()
{-# INLINE noop #-}

-- | Sequence two log messages one after another.  This operation has an
-- identity 'noop'.
append :: PushLog a -> PushLog a -> PushLog a
append (PushLog f) (PushLog g) =
    PushLog $ \a -> f a *> g a
{-# INLINE append #-}

#ifdef WITH_data_default_class
-- | Doesn't push a log message in to backend, only returns ().
instance Default (PushLog a) where
    def = noop
    {-# INLINE def #-}
#endif

-- | @('<>') = 'append'@
instance Semigroup (PushLog a) where
    (<>) = append
    {-# INLINE (<>) #-}

-- | @'mempty' = 'noop'@
instance Monoid (PushLog a) where
    mempty = noop
    {-# INLINE mempty #-}

#ifndef SEMIGROUP_MONOID
    mappend = (<>)
    {-# INLINE mappend #-}
#endif

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
