{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP, FlexibleInstances, DeriveDataTypeable, NoImplicitPrelude,
--               TypeFamilies
--
-- Code taken, and adapted, from:
-- <https://hackage.haskell.org/package/fast-logger fast-logger> created by
-- Kazu Yamamoto \<kazu@iij.ad.jp\> under
-- <https://github.com/kazu-yamamoto/logger/blob/master/fast-logger/LICENSE BSD3 license>.
module Data.Lumberjack.LogStr
  where

import Prelude (Num((+)))

import Data.Function ((.), id)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Monoid (Monoid(mempty, mappend))
import Data.String (IsString(fromString), String)
import Data.Typeable (Typeable)
import Data.Word (Word, Word16, Word32, Word64, Word8)

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString as Strict.ByteString (concat, empty, length)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
    ( byteString
    , intDec
    , int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , toLazyByteString
    , wordDec
    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    )
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString (toChunks, toStrict)
import qualified Data.Text as Strict (Text)
--import qualified Data.Text as Strict.Text (pack)
import qualified Data.Text.Encoding as Strict.Text (encodeUtf8)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (pack)
import qualified Data.Text.Lazy.Encoding as Lazy.Text (encodeUtf8)

import Data.NumberLength
    ( NumberLength(numberLength)
    , SignedNumberLength(signedNumberLength)
    )


-- | Log message builder. Use ('Data.Monoid.<>') to append two LogStr in O(1).
data LogStr = LogStr !Int Builder
  deriving Typeable

instance Monoid LogStr where
    mempty = LogStr 0 (Builder.byteString Strict.ByteString.empty)

    LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 `mappend` b2)

instance IsString LogStr where
    fromString = toLogStr . Lazy.Text.pack

-- | Obtaining the length of 'LogStr' in O(1).
logStrLength :: LogStr -> Int
logStrLength (LogStr n _) = n

-- -- | Converting 'LogStr' to 'ByteString'.
fromLogStr :: LogStr -> Strict.ByteString
fromLogStr (LogStr _ builder) = fromBuilder builder
  where
    fromBuilder = toStrictByteString . Builder.toLazyByteString
    toStrictByteString =
#if MIN_VERSION_bytestring(0,10,0)
        Lazy.ByteString.toStrict
#else
        Strict.ByteString.concat . Lazy.ByteString.toChunks
#endif

-- {{{ ToLogStr ---------------------------------------------------------------

class ToLogStr msg where
    toLogStr :: msg -> LogStr

instance ToLogStr LogStr where
    toLogStr = id

instance ToLogStr Strict.ByteString where
    toLogStr bs = LogStr (Strict.ByteString.length bs) (Builder.byteString bs)

instance ToLogStr Lazy.ByteString where
    toLogStr = toLogStr . Strict.ByteString.concat . Lazy.ByteString.toChunks

instance ToLogStr String where
    toLogStr = toLogStr . Lazy.Text.pack

instance ToLogStr Strict.Text where
    toLogStr = toLogStr . Strict.Text.encodeUtf8

instance ToLogStr Lazy.Text where
    toLogStr = toLogStr . Lazy.Text.encodeUtf8

instance ToLogStr Int where
    toLogStr n = LogStr (signedNumberLength n) (Builder.intDec n)

instance ToLogStr Int8 where
    toLogStr n = LogStr (signedNumberLength n) (Builder.int8Dec n)

instance ToLogStr Int16 where
    toLogStr n = LogStr (signedNumberLength n) (Builder.int16Dec n)

instance ToLogStr Int32 where
    toLogStr n = LogStr (signedNumberLength n) (Builder.int32Dec n)

instance ToLogStr Int64 where
    toLogStr n = LogStr (signedNumberLength n) (Builder.int64Dec n)

instance ToLogStr Word where
    toLogStr n = LogStr (numberLength n) (Builder.wordDec n)

instance ToLogStr Word8 where
    toLogStr n = LogStr (numberLength n) (Builder.word8Dec n)

instance ToLogStr Word16 where
    toLogStr n = LogStr (numberLength n) (Builder.word16Dec n)

instance ToLogStr Word32 where
    toLogStr n = LogStr (numberLength n) (Builder.word32Dec n)

instance ToLogStr Word64 where
    toLogStr n = LogStr (numberLength n) (Builder.word64Dec n)

-- }}} ToLogStr ---------------------------------------------------------------

-- {{{ LogStr -----------------------------------------------------------------

-- | Construct 'LogStr' from multiple pieces that have instance of 'ToLogStr'
-- type class.
logStr :: (LogStrArgs args) => args
logStr = logStrArgs mempty

-- | Class describes variadic arguments of 'logStr' function.
class LogStrArgs a where
    type Result a

    logStrArgs :: LogStr -> a

instance LogStrArgs LogStr where
    type Result LogStr = LogStr

    logStrArgs = id

instance (ToLogStr a, LogStrArgs r) => LogStrArgs (a -> r) where
    type Result (a -> r) = r

    logStrArgs str a = logStrArgs (str `mappend` toLogStr a)

-- }}} LogStr -----------------------------------------------------------------
