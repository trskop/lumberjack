{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

#ifdef WITH_mtl
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2018 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Control.Monad.Logger.Internal
  where

import Control.Applicative (Applicative((<*>), pure))
import Control.Monad (Monad((>>=), return))
import Data.Function ((.), ($))
import Data.Functor (Functor(fmap))
import Data.Typeable (Typeable)
import System.IO (IO)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))

#if WITH_mtl
import Control.Monad.Cont.Class (MonadCont(callCC))
import Control.Monad.Error.Class (MonadError(catchError, throwError))
import Control.Monad.Reader.Class (MonadReader(ask, local, reader))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState(get, put, state))
import Control.Monad.Writer.Class (MonadWriter(listen, pass, tell, writer))
#endif

#if WITH_exceptions
import Control.Monad.Catch
    ( MonadCatch
        ( catch
#if MIN_VERSION_exceptions(0,6,0)
        )
    -- In exceptions >=0.6 MonadCatch type class was split in to MonadCatch and
    -- MonadMask type classes.
    , MonadMask
        ( mask
#else
        , mask
#endif
    -- MIN_VERSION_exceptions(0,6,0)

#if MIN_VERSION_exceptions(0,2,0)
        -- Method uninterruptibleMask was introduced in exceptions >=0.2,
        -- originally as part of MonadCatch, later (>=0.6) as part of MonadMask
        -- type class.
        , uninterruptibleMask
#endif
    -- MIN_VERSION_exceptions(0,2,0)

#if MIN_VERSION_exceptions(0,4,0)
        )
    -- Type class MonadThrow was factored out of MonadCatch in exceptions 0.4.
    , MonadThrow(throwM)
#endif
    -- MIN_VERSION_exceptions(0,4,0)
    )
#endif
    -- WITH_exceptions

import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))

import Control.Monad.Logger.Class
    ( MonadLogger(runPushLog)
    , MonadLoggerIO(askRunPushLog)
    )
import System.Lumberjack.Backend
    ( LoggingBackend
    , SomeLoggingBackend(SomeLoggingBackend)
    )
import System.Lumberjack.PushLog (PushLog)
import qualified System.Lumberjack.PushLog as PushLog (runPushLog)


-- | Monad transformer that adds logging capability.
newtype LoggingT m a = LoggingT
    { _runLoggingT :: (PushLog SomeLoggingBackend -> IO ()) -> m a
    -- ^ See also '_evalLoggingT', 'runLoggingT' and 'evalLoggingT'.
    }
  deriving (Typeable)

-- | Flipped version of '_runLoggingT'. See also 'evalLoggingT' and
-- 'runLoggingT'.
_evalLoggingT
    :: (PushLog SomeLoggingBackend -> IO ())
    -> LoggingT m a
    -> m a
_evalLoggingT r (LoggingT f) = f r

-- | Evaluate 'LoggingT' monad transformer in to underlying monad using
-- provided logging backend. There is also flipped version named
-- 'evalLoggingT'.
runLoggingT
    :: LoggingBackend b
    => LoggingT m a
    -> b
    -- ^ Logging backend to use when evaluating 'LoggingT' monad transformer.
    -> m a
runLoggingT m = withRunPushLog $ _runLoggingT m
-- Unable to reduce in to "withRunPushLog . _runLoggingT" due to RankNTypes.

-- | Evaluate 'LoggingT' monad transformer in to underlying monad using
-- provided logging backend. There is also flipped version named 'runLoggingT'.
evalLoggingT
    :: LoggingBackend b
    => b
    -- ^ Logging backend to use when evaluating 'LoggingT' monad transformer.
    -> LoggingT m a
    -> m a
evalLoggingT = withRunPushLog _evalLoggingT

-- | Helper function that simplifies implementation of 'runLoggingT' and
-- 'evalLoggingT' functions.
withRunPushLog
    :: LoggingBackend b
    => ((PushLog SomeLoggingBackend -> IO ()) -> a)
    -- ^ Low-level evaluation function for 'LoggingT' monad transformer.
    -- See '_runLoggingT' and '_evalLoggingT'.
    -> b
    -- ^ Logging backend to use when evaluating 'LoggingT' monad transformer.
    -> a
withRunPushLog f backend = f (PushLog.runPushLog (SomeLoggingBackend backend))
{-# INLINE withRunPushLog #-}

-- {{{ Transformer lifting operations -----------------------------------------

liftLoggingT :: m a -> LoggingT m a
liftLoggingT x = LoggingT $ \_ -> x
{-# INLINE liftLoggingT #-}
{-# ANN liftLoggingT "HLint: ignore Use const" #-}
-- Using const is not possible due to RankNTypes.

mapLoggingT :: (m a -> m b) -> LoggingT m a -> LoggingT m b
mapLoggingT f (LoggingT g) = LoggingT $ \r -> f (g r)
-- Can not reduce "\r -> f (g r)" to "f . g" due to RankNTypes.
{-# INLINE mapLoggingT #-}

mapLoggingT2
    :: (m a -> m b -> m c)
    -> LoggingT m a -> LoggingT m b -> LoggingT m c
mapLoggingT2 f (LoggingT g) (LoggingT h) = LoggingT $ \r -> g r `f` h r
{-# INLINE mapLoggingT2 #-}

mapLoggingT3
    :: (m a -> m b -> m c -> m d)
    -> LoggingT m a -> LoggingT m b -> LoggingT m c -> LoggingT m d
mapLoggingT3 f (LoggingT g) (LoggingT h) (LoggingT i) =
    LoggingT $ \r -> f (g r) (h r) (i r)
{-# INLINE mapLoggingT3 #-}

liftBindLike
    :: (m a -> (b -> m c) -> m d)
    -> LoggingT m a -> (b -> LoggingT m c) -> LoggingT m d
liftBindLike f (LoggingT g) h = LoggingT $ \r -> f (g r) (_evalLoggingT r . h)
{-# INLINE liftBindLike #-}

liftMask
    :: (((forall a. m a -> m a) -> m b) -> m b)
    -> ((forall a. LoggingT m a -> LoggingT m a) -> LoggingT m b)
    -> LoggingT m b
liftMask unliftedMask f = LoggingT $ \r ->
    unliftedMask $ \restore -> _runLoggingT (f (mapLoggingT restore)) r
{-# INLINE liftMask #-}

liftCallCCLike
    :: (((a -> m b) -> m c) -> m d)
    -> ((a -> LoggingT m b) -> LoggingT m c)
    -> LoggingT m d
liftCallCCLike f g =
    LoggingT $ \r -> f $ \h -> _runLoggingT (g (liftLoggingT . h)) r
{-# INLINE liftCallCCLike #-}

-- }}} Transformer lifting operations -----------------------------------------

instance Functor f => Functor (LoggingT f) where
    fmap = mapLoggingT . fmap

instance Applicative f => Applicative (LoggingT f) where
    pure = liftLoggingT . pure
    (<*>) = mapLoggingT2 (<*>)

instance Monad m => Monad (LoggingT m) where
    return = liftLoggingT . return
    (>>=) = liftBindLike (>>=)

-- {{{ MonadLogger and MonadLoggerIO instances --------------------------------

instance
    ( MonadIO m
#ifndef APPLICATIVE_MONAD
    , Applicative m
#endif
    )
    => MonadLogger (LoggingT m)
  where
    runPushLog pushLog = LoggingT $ \runPushLogIO ->
        liftIO $ runPushLogIO pushLog
    {-# INLINEABLE runPushLog #-}

instance
    ( MonadIO m
#ifndef APPLICATIVE_MONAD
    , Applicative m
#endif
    )
    => MonadLoggerIO (LoggingT m)
  where
    askRunPushLog = LoggingT $ \f -> return f
    {-# INLINEABLE askRunPushLog #-}
    -- Eta reduction is not possible due to RankNTypes.

-- }}} MonadLogger and MonadLoggerIO instances --------------------------------

-- {{{ Instances for transformers package -------------------------------------

instance MonadTrans LoggingT where
    lift = liftLoggingT
    {-# INLINE lift #-}

instance MonadIO m => MonadIO (LoggingT m) where
    liftIO = liftLoggingT . liftIO
    {-# INLINE liftIO #-}

-- }}} Instances for transformers package -------------------------------------

#ifdef WITH_mtl
-- {{{ Instances for mtl package ----------------------------------------------

instance MonadCont m => MonadCont (LoggingT m) where
    callCC = liftCallCCLike callCC

instance MonadError e m => MonadError e (LoggingT m) where
    throwError = liftLoggingT . throwError
    catchError = liftBindLike catchError

instance MonadReader r m => MonadReader r (LoggingT m) where
   ask = liftLoggingT ask
   local = mapLoggingT . local
   reader = liftLoggingT . reader

instance
    (MonadReader r m, MonadWriter w m, MonadState s m)
    => MonadRWS r w s (LoggingT m)

instance MonadState s m => MonadState s (LoggingT m) where
    get = liftLoggingT get
    put = liftLoggingT . put
    state = liftLoggingT . state

instance MonadWriter w m => MonadWriter w (LoggingT m) where
    listen = mapLoggingT listen
    pass = mapLoggingT pass
    tell = liftLoggingT . tell
    writer = liftLoggingT . writer

-- }}} Instances for mtl package ----------------------------------------------
#endif

#ifdef WITH_exceptions
-- {{{ Instances for exceptions package ---------------------------------------

instance MonadCatch m => MonadCatch (LoggingT m) where
    catch = liftBindLike catch

#if MIN_VERSION_exceptions(0,6,0)
-- In exceptions >=0.6 MonadCatch type class was split in to MonadCatch and
-- MonadMask type classes.

instance MonadMask m => MonadMask (LoggingT m) where
#endif

    mask = liftMask mask

#if MIN_VERSION_exceptions(0,2,0)
-- Method uninterruptibleMask was introduced in exceptions >=0.2, originally as
-- part of MonadCatch, later (>=0.6) as part of MonadMask type class.

    uninterruptibleMask = liftMask uninterruptibleMask
#endif

#if MIN_VERSION_exceptions(0,4,0)
-- Type class MonadThrow was factored out of MonadCatch in exceptions 0.4.

instance MonadThrow m => MonadThrow (LoggingT m) where
#endif

    -- Part of MonadCatch for exceptions <0.4 and part of MonadThrow for
    -- exceptions >=0.4.
    throwM = liftLoggingT . throwM

-- }}} Instances for exceptions package ---------------------------------------
#endif

-- {{{ Instances for mmorph package -------------------------------------------

instance MFunctor LoggingT where
    -- :: Monad m
    -- => (forall a. m a -> n a)
    -- -> LoggingT m b -> LoggingT n b
    hoist f (LoggingT g) = LoggingT $ \r -> f (g r)

instance MMonad LoggingT where
    -- :: Monad n
    -- => (forall a. m a -> LoggingT n a)
    -- -> LoggingT m b -> LoggingT n b
    embed f (LoggingT g) = LoggingT $ \r -> _runLoggingT (f (g r)) r

-- }}} Instances for mmorph package -------------------------------------------
