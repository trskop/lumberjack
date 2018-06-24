{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module:      System.Lumberjack.Boilerplate
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module System.Lumberjack.Boilerplate
    ( LogFunctions(..)
    )
  where

import GHC.Generics (Generic)


-- | Intended to be used as a result of smart constructors, e.g.:
--
-- @
-- 'logError', 'logWarning', 'logInfo', 'logDebug', 'logTrace'
--     :: MyLoggingFunction
--
-- 'LogFunctions'{..} <- myLoggingFunctions
-- @
data LogFunctions a b c d e = LogFunctions
    { logError :: a
    , logWarning :: b
    , logInfo :: c
    , logDebug :: d
    , logTrace :: e
    }
  deriving (Generic)
