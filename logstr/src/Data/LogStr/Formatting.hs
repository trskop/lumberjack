{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Inspired by <https://hackage.haskell.org/package/formatting formatting>
-- package. Kept the same naming conventions to make drop-in replecement
-- possible.
module Data.LogStr.Formatting
    ( Format
    , (%)
    , bind
    , later
    , map
    , now
    , run
    , runFormat
    , format
    , something
    )
  where

import Data.LogStr.Class (ToLogStr(toLogStr))
import Data.LogStr.Internal (LogStr)

import Data.LogStr.Formatting.Internal
    ( Format
    , (%)
    , bind
    , later
    , map
    , now
    , run
    , runFormat
    )


-- | Evaluate 'Format' in to its accumulator type. In example:
--
-- @
-- 'now' ('toLogStr' \"some text\") :: 'Format' 'LogStr' 'LogStr'
--
-- 'run' $ 'now' ('toLogStr' \"some text\") :: 'LogStr'
-- @
--
-- @
-- int :: 'Format' r (Text -> r)
-- char :: 'Format' r (Char -> r)
-- int '%' char :: 'Format' r (Int -> (Char -> r))
--
-- 'format' $ int '%' char :: Int -> Char -> 'LogStr'
-- @
--
-- This function is just an alias for 'run'.
format :: Format LogStr a -> a
format = run
{-# INLINE format #-}

-- | Alias for @'later' 'toLogStr'@, which works for any 'ToLogStr' instance.
something :: ToLogStr a => Format r (a -> r)
something = later toLogStr
