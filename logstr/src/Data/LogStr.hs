{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP, FlexibleInstances, DeriveDataTypeable, NoImplicitPrelude,
--               TypeFamilies
--
-- This package is inspired by a lot of good packages that are out there:
--
-- * <https://hackage.haskell.org/package/fast-logger fast-logger> created by
--   Kazu Yamamoto \<kazu@iij.ad.jp\> under
--   <https://github.com/kazu-yamamoto/logger/blob/master/fast-logger/LICENSE BSD3 license>.
--
-- * <https://hackage.haskell.org/package/formatting formatting> created
-- Chris
--   Done \<chrisdone@gmail.com\>, and a lot of others, under
--   <https://github.com/chrisdone/formatting/blob/master/LICENSE>
module Data.LogStr
    (
    -- * LogStr Data Type
      LogStr
    , fromLogStr
    , empty
    , length
    , null

    -- * Conversion To LogStr
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
    , LogStrArgs(logStrArgs)
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
