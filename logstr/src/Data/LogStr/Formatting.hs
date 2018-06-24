{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Combinator-based type-safe formatting for logging messages.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  FlexibleContexts, NoImplicitPrelude
--
-- Combinator-based type-safe formatting (like printf() or FORMAT) for logging
-- messages.
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

    -- * Formatters
    , hex
    , int
    , shown
    , something
    , char
    , stext
    , string
    , text
    )
  where

import Prelude (Integral)

import Data.Char (Char)
import Data.Function ((.))
import Data.String (String)
import Text.Show (Show)

import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)

import Data.Tagged (Tagged)

import Data.LogStr.Class (Hexadecimal, ToLogStr(toLogStr))
import qualified Data.LogStr.Class as Class (hex, showed)
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

hex :: ToLogStr (Tagged Hexadecimal a) => Format r (a -> r)
hex = later (toLogStr . Class.hex)

int :: (Integral a, ToLogStr a) => Format r (a -> r)
int = something

char :: Format r (Char -> r)
char = something

shown :: Show a => Format r (a -> r)
shown = later (toLogStr . Class.showed)

string :: Format r (String -> r)
string = something

text :: Format r (Lazy.Text -> r)
text = something

stext :: Format r (Strict.Text -> r)
stext = something
