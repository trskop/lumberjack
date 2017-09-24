{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               NoImplicitPrelude, RankNTypes, TypeFamilies
--
-- TODO
module System.Lumberjack.PushLog
    ( PushLog(..)
    , mkPushLog
    , noop
    , runPushLog
    , runPushLogTaggedWith
    , forgetPushLogTag

    --
    , Str
    , Line
    , pushLog
    , pushLogLn
    )
  where

import Control.Monad (Monad((>>), return))
import Data.Function ((.), ($), const, flip)
import Data.Typeable (Typeable)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup (Semigroup((<>)))
import GHC.Generics (Generic, Generic1)
import System.IO (IO)

import Data.Default.Class (Default(def))

import System.Lumberjack.Backend (LoggingBackend, pushLogStr, pushLogStrLn)
import Data.LogStr (LogStrArgs(Result, logStrArgs), LogStr)


-- | Represents closure of a function like 'pushLogStr' or 'pushLogStrLn', with
-- already bounded 'LogStr' argument.
newtype PushLog b t = PushLog (b -> IO ())
  deriving (Generic, Generic1, Typeable)

noop :: PushLog b t
noop = PushLog . const $ return ()
{-# INLINE noop #-}

append :: PushLog b t -> PushLog b t -> PushLog b t
append (PushLog f) (PushLog g) =
    PushLog $ \backend -> f backend >> g backend
{-# INLINE append #-}

-- | Doesn't push a log message in to backend, only returns ().
instance Default (PushLog b t) where
    def = noop
    {-# INLINE def #-}

instance Semigroup (PushLog b t) where
    (<>) = append
    {-# INLINE (<>) #-}

instance Monoid (PushLog b t) where
    mempty = noop
    {-# INLINE mempty #-}

    mappend = append
    {-# INLINE mappend #-}

-- | Run 'PushLog' using provided logging backend.
runPushLog :: LoggingBackend b => b -> PushLog b t -> IO ()
runPushLog backend (PushLog f) = f backend
{-# INLINE runPushLog #-}

-- | Variant of 'runPushLog' where type variable @t@ in @'PushLog' b t@ can be
-- restricted by a type proxy. Implemented as:
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
forgetPushLogTag (PushLog f) = PushLog f    -- = coerce
{-# INLINE forgetPushLogTag #-}

data Str
  deriving (Generic, Typeable)

data Line
  deriving (Generic, Typeable)

-- | Run 'PushLog' using provided logging backend without appending new line at
-- the end of the log message.
pushLog :: LoggingBackend b => b -> PushLog b Str -> IO ()
pushLog = runPushLogTaggedWith (Proxy :: Proxy Str)
{-# INLINE pushLog #-}

-- | Run 'PushLog' using provided logging backend with new line appended to the
-- end of the log message.
pushLogLn :: LoggingBackend b => b -> PushLog b Line -> IO ()
pushLogLn = runPushLogTaggedWith (Proxy :: Proxy Line)
{-# INLINE pushLogLn #-}

instance LoggingBackend b => LogStrArgs (PushLog b Str) where
    type Result (PushLog b Str) = PushLog b Str

    logStrArgs = mkPushLog pushLogStr
    {-# INLINE logStrArgs #-}

instance LoggingBackend b => LogStrArgs (PushLog b Line) where
    type Result (PushLog b Line) = PushLog b Line

    logStrArgs = mkPushLog pushLogStrLn
    {-# INLINE logStrArgs #-}
