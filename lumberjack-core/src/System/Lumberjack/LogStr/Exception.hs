{-# LANGUAGE CPP #-}
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
-- Portability:  CPP, DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               NoImplicitPrelude, TypeFamilies
--
-- TODO
module System.Lumberjack.LogStr.Exception
  where

import Control.Exception
    ( Exception
        ( fromException
        , toException
#if MIN_VERSION_base(4,8,0)
        , displayException
        )
#endif
    , SomeAsyncException(SomeAsyncException)
    , SomeException(SomeException)
    )
import Data.Function ((.), const)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (typeRep)
#if !MIN_VERSION_base(4,8,0)
import Text.Show (Show(show))
#endif

import System.Lumberjack.LogStr
    ( LogStr
    , LogStrArgs(logStrArgs)
    , ToLogStr(toLogStr)
    , empty
    )


-- | Serializes exception in to a log message using following format:
--
-- @
-- {Exception \<exception-type\>: \<exception-message\>}
-- @
logStrException :: (Exception e, LogStrArgs args) => e -> args
logStrException e =
    logStrArgs empty "{Exception " exceptionType ": " exceptionStr "}"
  where
    exceptionType = showExceptionType e
    exceptionStr = showException e
        -- XXX: What if this contains new line characters?

-- | This uses type reflection ('Typeable') to get type information of
-- specified exception. It also takes care of the fact that 'SomeException' and
-- 'SomeAsyncException' are just containers and therefore should not be
-- presented as an exception type.
--
-- This function won't work correctly for custom exception hierarchies, since
-- it doesn't have any means to understand them.
showExceptionType :: Exception e => e -> LogStr
showExceptionType = go . toException
    -- Exception could be SomeException or some other exception, but we have no
    -- way to distinguish between those two situations without using Typeable
    -- hackery. Performing toException operation will result in only one case,
    -- SomeException.
  where
    go :: SomeException -> LogStr
    go _someException@(SomeException e) =
    -- Using underscore in _someException will supress warnings when base <4.7
    -- is in use.
#if MIN_VERSION_base(4,7,0)
        case fromException _someException :: Maybe SomeAsyncException of
            Just (SomeAsyncException e') -> typeOfException e'
            Nothing ->
#endif
                typeOfException e

    typeOfException :: Exception e => e -> LogStr
    typeOfException = toLogStr . typeRep . toProxy

    toProxy :: a -> Proxy a
    toProxy = const Proxy

-- | Helper function for converting 'Exception' in to 'String'; it also
-- provides compatibility layer for various versions of base package.
showException :: Exception e => e -> LogStr
showException = toLogStr . go
  where
    go =
#if MIN_VERSION_base(4,8,0)
        displayException
#else
        show
#endif
