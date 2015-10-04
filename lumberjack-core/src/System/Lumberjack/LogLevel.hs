{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               LambdaCase, NoImplicitPrelude, TemplateHaskell
--
-- Code taken, and adapted, from:
-- <https://hackage.haskell.org/package/monad-logger monad-logger> created by
--  Michael Snoyman \<michael@snoyman.com\> under
-- <https://github.com/kazu-yamamoto/logger/blob/master/monad-logger/LICENSE MIT license>.
module System.Lumberjack.LogLevel
  where

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function ((.), ($))
import qualified Data.List as List (drop)
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show(show))

import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import Language.Haskell.TH.Syntax (Lift(lift))

import System.Lumberjack.LogStr (LogStr, ToLogStr(toLogStr))


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
    | LevelOther Text
    -- ^ User defined logging level.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

defaultToText :: LogLevel -> Text
defaultToText = \case
    LevelOther t -> t
    level        -> Text.pack . List.drop 5 $ show level

defaultToLogStr :: LogLevel -> LogStr
defaultToLogStr = \case
    LevelOther t -> toLogStr t
    level        -> toLogStr . ByteString.pack . List.drop 5 $ show level

instance ToLogStr LogLevel where
    toLogStr = defaultToLogStr

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
        LevelOther x   -> [|LevelOther $ Text.pack $(lift $ Text.unpack x)|]
