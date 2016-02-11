{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Logging message builder with O(1) length operation.
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP, FlexibleInstances, DeriveDataTypeable, NoImplicitPrelude,
--               TypeFamilies
--
-- Logging message builder ('LogStr') with O(1) length operation. This module
-- only re-exports definitions from other modules.
--
-- This package is inspired by a lot of good packages that are out there:
--
-- * The <https://hackage.haskell.org/package/fast-logger fast-logger> package
--   created by Kazu Yamamoto under
--   <https://github.com/kazu-yamamoto/logger/blob/master/fast-logger/LICENSE BSD3 license>.
--
-- * The <https://hackage.haskell.org/package/formatting formatting> package
--   created Chris Done, and a lot of others, under
--   <https://github.com/chrisdone/formatting/blob/master/LICENSE BSD3 license>.
module Data.LogStr
    (
    -- * Logging Message Builder
    --
    -- | Similar to any other string builder, but it provides O(1) length
    -- operation to support more efficient I/O buffering.
      LogStr
    , fromLogStr
    , empty
    , length
    , null

    -- * Converting Values To LogStr
    , ToLogStr(..)

    -- ** Hexadecimal Representation
    , Hexadecimal
    , hex
    , hex1
    , hex2

    -- ** Conversion Using Show Instances
    , Showed
    , showed
    , showed1
    , showed2

    -- * Generic Logging Function
    , LogStrArgs(..)
    , logStr

    -- * Formatting Logging Message
    , Format
    , (%)
    , now
    , later
    , something
    , format
    )
  where

import Data.LogStr.Formatting
    ( Format
    , (%)
    , now
    , later
    , something
    , format
    )
import Data.LogStr.Internal
    ( LogStr
    , empty
    , fromLogStr
    , length
    , null
    )
import Data.LogStr.Class
    ( Hexadecimal
    , LogStrArgs(Result, logStrArgs)
    , Showed
    , ToLogStr(toLogStr)
    , hex
    , hex1
    , hex2
    , logStr
    , showed
    , showed1
    , showed2
    )
