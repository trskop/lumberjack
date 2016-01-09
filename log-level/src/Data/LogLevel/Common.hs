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
module Data.LogLevel.Common
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
import qualified Data.CaseInsensitive as CI (mk, original)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import Language.Haskell.TH.Syntax (Lift(lift))


-- | Commonly used log levels in production systems.
--
-- Description of individual log levels is based on Stack Overflow answer:
-- <https://stackoverflow.com/questions/7839565/logging-levels-logback-rule-of-thumb-to-assign-log-levels/8021604#8021604>
data LogLevel
    = LevelError
    -- ^ The system is in trouble, its functionality is affected. Customers are
    -- probably being affected (or will soon be) and the fix probably requires
    -- human intervention.

    | LevelWarning
    -- ^ An unexpected technical or business event happened, functionality of
    -- the system may be affected, and as a consequence also customers may be
    -- affected as well, but probably no immediate human intervention is
    -- required.
    --
    -- These log messages should be reviewed, and fixed, regardless of the fact
    -- if they directly affect system functionality.

    | LevelInfo
    -- ^ Messages about normal system operations. Things we want to see at high
    -- volume in case we need to forensically analyze an issue. System and
    -- \"session\" lifecycle events (system start, stop, login, logout, client
    -- connected to system RPC API, etc.) go here. Significant boundary events
    -- should be considered as well (e.g. database calls, remote API calls).
    -- Typical business exceptions can go here (e.g. login failed due to bad
    -- credentials). Any other event you think you'll need to see in production
    -- at high volume goes here.
    --
    -- If there is a need to visualize system load in terms of e.g. processed
    -- remote API calls per hour, then usually 'LevelInfo' messages are used
    -- for this purpose.

    | LevelDebug
    -- ^ Messges useful for debugging, especially during development and QA
    -- phases. While 'LevelError', 'LevelWarning', and 'LevelInfo' should be
    -- considered customer\/administrator\/technician oriented messages,
    -- 'LogDebug' is more for those people who must deal with the system
    -- internals.
    --
    -- If customer\/administrator\/technician requires support from the people
    -- who build the system these are usually the messages that are required
    -- for a successful fix.
    --
    -- Example of messages that should go in to 'LevelDebug':
    --
    -- * Message that are helpful in tracking the message\/information\/call
    --   flow through the system.
    --
    -- * Messages that help isolating potential issues, especially during the
    --   development and QA phases.
    --
    -- * Entry\/exit of non-trivial methods and marking interesting events and
    --   decision points inside methods.

    | LevelTrace
    -- ^ Very detailed messges useful for debugging. Use this sparingly, since
    -- these messages are usually very detailed with high information volume.
    --
    -- Messages traced on this level are typically not enabled during normal
    -- system operation, not even normal development cycle. Examples include
    -- dumping a full object hierarchy, logging some state during every
    -- iteration of a large loop, etc.

    | LevelOther !(CI Text)
    -- ^ User defined logging level. Try not to use these as it makes atomatic
    -- log analysis harder.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Convert (common) 'LogLevel' into (case insensitive) 'Text'.
toText :: LogLevel -> CI Text
toText = \case
    LevelOther t -> t
    level        -> CI.mk . Text.pack . List.drop 5 $ show level
{-# INLINEABLE toText #-}

instance Lift LogLevel where
    lift = \case
        LevelError   -> [|LevelError|]
        LevelWarning -> [|LevelWarning|]
        LevelInfo    -> [|LevelInfo|]
        LevelDebug   -> [|LevelDebug|]
        LevelTrace   -> [|LevelTrace|]
        LevelOther x -> [|LevelOther . CI.mk
            $ Text.pack $(lift . Text.unpack $ CI.original x)|]
