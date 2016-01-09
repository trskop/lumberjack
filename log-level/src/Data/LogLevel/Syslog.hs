{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               LambdaCase, NoImplicitPrelude, TemplateHaskell
--
-- TODO
module Data.LogLevel.Syslog
  where

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function ((.), ($))
import qualified Data.List as List (drop)
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show(show))

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI (mk)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import Language.Haskell.TH.Syntax (Lift(lift))

import System.Lumberjack.LogStr (ToLogStr(toLogStr))


-- | Log message priority, based on @syslog(3)@ from
-- <http://www.opengroup.org/onlinepubs/009695399/basedefs/syslog.h.html POSIX.1-2001>.
data LogLevel
    = LevelEmergency
    -- ^ A panic condition; system is unusable.
    | LevelAlert
    -- ^ Action must be taken immediately.
    | LevelCritical
    -- ^ Critical condition.
    | LevelError
    -- ^ Error condition.
    | LevelWarning
    -- ^ Warning condition.
    | LevelNotice
    -- ^ Normal but significant condition that requires special handling.
    | LevelInfo
    -- ^ General information messages.
    | LevelDebug
    -- ^ Messges useful for debugging.
    | LevelOther !(CI Text)
    -- ^ User defined logging level.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Convert (syslog) 'LogLevel' into (case insensitive) 'Text'.
toText :: LogLevel -> CI Text
toText = \case
    LevelOther t -> t
    level        -> CI.mk . Text.pack . List.drop 5 $ show level
{-# INLINEABLE toText #-}

instance ToLogStr LogLevel where
    toLogStr = toLogStr . toText
    {-# INLINEABLE toLogStr #-}

instance Lift LogLevel where
    lift = \case
        LevelEmergency -> [|LevelEmergency|]
        LevelAlert     -> [|LevelAlert|]
        LevelCritical  -> [|LevelCritical|]
        LevelError     -> [|LevelError|]
        LevelWarning   -> [|LevelWarning|]
        LevelNotice    -> [|LevelNotice|]
        LevelInfo      -> [|LevelInfo|]
        LevelDebug     -> [|LevelDebug|]
        LevelOther x   ->
            [|LevelOther . CI.mk $ Text.pack $(lift $ Text.unpack x)|]
