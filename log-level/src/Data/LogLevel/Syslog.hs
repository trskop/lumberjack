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
module Data.LogLevel.Syslog
    (
    -- * Syslog LogLevel Data Type
      LogLevel(..)
    , defaultLogLevel

    -- * Conversion From and To LogLevel
    , toString
    , fromString
    )
  where

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Maybe (Maybe(Just, Nothing))
import qualified Data.List as List (drop)
import Data.Ord (Ord)
import Data.String (IsString)
import qualified Data.String as String (IsString(fromString))
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show(show))

import Data.CaseInsensitive (FoldCase)
import qualified Data.CaseInsensitive as CI (mk)
import Language.Haskell.TH.Syntax (Lift(lift))

import Data.Default.Class (Default(def))

import Data.LogLevel.Classes (FromString, ToString)
import qualified Data.LogLevel.Classes (FromString(fromString), ToString(toString))


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
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Convert (syslog) 'LogLevel' into a string.
toString :: IsString string => LogLevel -> string
toString = String.fromString . List.drop 5 . show
{-# INLINEABLE toString #-}

instance ToString LogLevel where
    toString = toString
    {-# INLINE toString #-}

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

-- | Default 'LogLevel' value is 'LevelNotice'. This is the same default value
-- as e.g. Linux @logger@ command uses.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelNotice
{-# INLINE defaultLogLevel #-}

-- | @'def' = 'defaultLogLevel' = 'LevelNotice'@
instance Default LogLevel where
    def = defaultLogLevel
    {-# INLINE def #-}

-- | Convert a string in to 'LogLevel'. Conversion is done as follows:
--
-- @
-- \"\"          -> 'Just' 'defaultLogLevel'
-- \"emergency\" -> 'Just' 'LevelEmergency'
-- \"alert\"     -> 'Just' 'LevelAlert'
-- \"critical\"  -> 'Just' 'LevelCritical'
-- \"error\"     -> 'Just' 'LevelError'
-- \"warning\"   -> 'Just' 'LevelWarning'
-- \"notice\"    -> 'Just' 'LevelNotice'
-- \"info\"      -> 'Just' 'LevelInfo'
-- \"debug\"     -> 'Just' 'LevelDebug'
-- @
--
-- String comparison is done in case insensitive manner, and any other value
-- (which is not described above) is mapped in to 'Nothing'.
fromString
    :: (Eq string, FoldCase string, IsString string)
    => string
    -> Maybe LogLevel
fromString str = case CI.mk str of
    "" -> Just defaultLogLevel
    "emergency" -> Just LevelEmergency
    "alert" -> Just LevelAlert
    "critical" -> Just LevelCritical
    "error" -> Just LevelError
    "warning" -> Just LevelWarning
    "notice" -> Just LevelNotice
    "info" -> Just LevelInfo
    "debug" -> Just LevelDebug
    _ -> Nothing
{-# INLINE fromString #-}

instance FromString LogLevel where
    fromString = fromString
    {-# INLINE fromString #-}
