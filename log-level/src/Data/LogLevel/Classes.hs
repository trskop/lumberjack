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
-- TODO
module Data.LogLevel.Classes
    (
    -- * Convert a string in to a LogLevel
      FromString(..)

    -- * Convert a LogLevel in to a string
    , ToString(..)
    )
  where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Maybe (Maybe)
import Data.String (IsString)

import Data.CaseInsensitive (FoldCase)


class FromString a where
    fromString
        :: (Eq string, Ord string, FoldCase string, IsString string)
        => string
        -> Maybe a

class ToString a where
    toString :: (IsString string) => a -> string
