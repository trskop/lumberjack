{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               LambdaCase, NoImplicitPrelude, OverloadedStrings,
--               TemplateHaskell
--
-- TODO
module Data.LogLevel.Extensible
    (
    -- * Syslog LogLevel Data Type
      ExtensibleLogLevel(..)
    , defaultLogLevel

    -- * Conversion From and To LogLevel
    , toString
    , fromString
    )
  where

import Data.Bool (otherwise)
import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function ((.), ($))
import Data.Maybe (Maybe(Just))
import Data.Ord (Ord)
import Data.String (IsString, String)
import qualified Data.String as String (IsString(fromString))
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

import Data.CaseInsensitive (CI, FoldCase)
import qualified Data.CaseInsensitive as CI (mk, original)
import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text (pack, unpack)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (pack, unpack)
import Language.Haskell.TH.Syntax (Lift(lift))

import Data.Default.Class (Default(def))

import Data.LogLevel.Classes as Classes (FromString)
import qualified Data.LogLevel.Classes as Classes (FromString(fromString))


data ExtensibleLogLevel string logLevel
    = LevelKnown logLevel
    -- ^ Already defined log level in type @logLevel@.
    | LevelOther !(CI string)
    -- ^ User defined logging level.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Convert 'ExtensibleLogLevel' into a case insensitive string.
toString
    :: (logLevel -> CI string)
    -> ExtensibleLogLevel string logLevel
    -> CI string
toString logLevelToString = \case
    LevelOther t -> t
    LevelKnown l -> logLevelToString l
{-# INLINEABLE toString #-}

instance Lift logLevel => Lift (ExtensibleLogLevel String logLevel) where
    lift = \case
        LevelKnown l -> [|LevelKnown $(lift l)|]
        LevelOther s -> [|LevelOther $ CI.mk $(lift $ CI.original s)|]

instance Lift logLevel => Lift (ExtensibleLogLevel Strict.Text logLevel) where
    lift = \case
        LevelKnown l -> [|LevelKnown $(lift l)|]
        LevelOther s -> [|LevelOther . CI.mk
            $ Strict.Text.pack $(lift . Strict.Text.unpack $ CI.original s)|]

instance Lift logLevel => Lift (ExtensibleLogLevel Lazy.Text logLevel) where
    lift = \case
        LevelKnown l -> [|LevelKnown $(lift l)|]
        LevelOther s -> [|LevelOther . CI.mk
            $ Lazy.Text.pack $(lift . Lazy.Text.unpack $ CI.original s)|]

-- | Default 'ExtensibleLogLevel' value is:
--
-- @
-- 'LevelKnown' 'def'
-- @
defaultLogLevel :: Default logLevel => ExtensibleLogLevel string logLevel
defaultLogLevel = LevelKnown def
{-# INLINE defaultLogLevel #-}

-- | @'def' = 'LevelKnown' 'def'@
instance Default logLevel => Default (ExtensibleLogLevel string logLevel) where
    def = defaultLogLevel
    {-# INLINE def #-}

-- | TODO
fromString
    :: (Eq s, FoldCase s, IsString s, Ord s)
    => (s -> Maybe logLevel)
    -> s
    -> ExtensibleLogLevel s logLevel
fromString logLevelFromString str
  | Just logLevel <- logLevelFromString str = LevelKnown logLevel
  | otherwise                               = LevelOther $ CI.mk str
{-# INLINE fromString #-}

instance
    ( Eq string
    , FoldCase string
    , FromString logLevel
    , IsString string
    , Ord string
    )
    => IsString (ExtensibleLogLevel string logLevel)
  where
    fromString = fromString Classes.fromString . String.fromString
    {-# INLINE fromString #-}
