{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Based on code from:
-- <https://hackage.haskell.org/package/fast-logger fast-logger> created by
-- Kazu Yamamoto \<kazu@iij.ad.jp\> under
-- <https://github.com/kazu-yamamoto/logger/blob/master/fast-logger/LICENSE BSD3 license>.
module System.Lumberjack.LogStr
    ( LogStr(..)
    , fromLogStr
    , logStrLength

    , ToLogStr(..)
    , Hexadecimal
    , hex

    , LogStrArgs(..)
    , logStr
    )
  where

import Prelude (Num((+)))

import Data.Function ((.), ($), id)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Monoid (Monoid(mappend, mempty))
import Data.String (IsString(fromString), String)
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(Proxy))
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString as Strict.ByteString
    ( empty
    , length
#if !MIN_VERSION_bytestring(0,10,0)
    , concat
#endif
    )
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
    ( byteString
    , int16Dec
    , int16HexFixed
    , int32Dec
    , int32HexFixed
    , int64Dec
    , int64HexFixed
    , int8Dec
    , int8HexFixed
    , intDec
    , toLazyByteString
    , word16Dec
    , word16Hex
    , word32Dec
    , word32Hex
    , word64Dec
    , word64Hex
    , word8Dec
    , word8Hex
    , wordDec
    , wordHex
    )
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
#if MIN_VERSION_bytestring(0,10,0)
    (toStrict)
#else
    (toChunks)
#endif
import qualified Data.Text as Strict (Text)
--import qualified Data.Text as Strict.Text (pack)
import qualified Data.Text.Encoding as Strict.Text (encodeUtf8)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (pack)
import qualified Data.Text.Lazy.Encoding as Lazy.Text (encodeUtf8)

import Data.NumberLength
    ( BoundedNumberLength(maxNumberLengthHex)
    , NumberLength(numberLength, numberLengthHex)
    , SignedNumberLength(signedNumberLength)
    )
import Data.Tagged (Tagged(Tagged))


-- | Log message builder. Monoid operations (including concatenation) are in
-- O(1) and so is 'logStrLength'.
data LogStr = LogStr !Int Builder
  deriving (Generic, Typeable)

instance Monoid LogStr where
    mempty = LogStr 0 (Builder.byteString Strict.ByteString.empty)

    LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 `mappend` b2)

instance IsString LogStr where
    fromString = toLogStr . Lazy.Text.pack

-- | Obtaining the length of 'LogStr' in O(1).
logStrLength :: LogStr -> Int
logStrLength (LogStr n _) = n

-- | Convert 'LogStr' to strict 'ByteString'.
fromLogStr :: LogStr -> Strict.ByteString
fromLogStr (LogStr _ builder) = fromBuilder builder
  where
    fromBuilder = toStrictByteString . Builder.toLazyByteString

toStrictByteString :: Lazy.ByteString -> Strict.ByteString
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
    toLogStr = toLogStr . toStrictByteString

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

-- {{{ Hexadecimal ------------------------------------------------------------

data Hexadecimal
  deriving (Generic, Typeable)

-- | Mark value as 'Hexadecimal' when converting it to 'LogStr'.
hex :: a -> Tagged Hexadecimal a
hex = Tagged

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

{- TODO: Find elegant way how to get around not having Builder.intHexFixed
instance ToLogStr (Tagged Hexadecimal Int) where
    toLogStr n = LogStr (signedNumberLength n) (Builder.intDec n)
-}

instance ToLogStr (Tagged Hexadecimal Int8) where
    toLogStr (Tagged n) =
        LogStr (maxNumberLengthHex $ proxyOf n) $ Builder.int8HexFixed n

instance ToLogStr (Tagged Hexadecimal Int16) where
    toLogStr (Tagged n) =
        LogStr (maxNumberLengthHex $ proxyOf n) $ Builder.int16HexFixed n

instance ToLogStr (Tagged Hexadecimal Int32) where
    toLogStr (Tagged n) =
        LogStr (maxNumberLengthHex $ proxyOf n) $ Builder.int32HexFixed n

instance ToLogStr (Tagged Hexadecimal Int64) where
    toLogStr (Tagged n) =
        LogStr (maxNumberLengthHex $ proxyOf n) $ Builder.int64HexFixed n

instance ToLogStr (Tagged Hexadecimal Word) where
    toLogStr (Tagged n) = LogStr (numberLengthHex n) (Builder.wordHex n)

instance ToLogStr (Tagged Hexadecimal Word8) where
    toLogStr (Tagged n) = LogStr (numberLengthHex n) (Builder.word8Hex n)

instance ToLogStr (Tagged Hexadecimal Word16) where
    toLogStr (Tagged n) = LogStr (numberLengthHex n) (Builder.word16Hex n)

instance ToLogStr (Tagged Hexadecimal Word32) where
    toLogStr (Tagged n) = LogStr (numberLengthHex n) (Builder.word32Hex n)

instance ToLogStr (Tagged Hexadecimal Word64) where
    toLogStr (Tagged n) = LogStr (numberLengthHex n) (Builder.word64Hex n)

-- }}} Hexadecimal ------------------------------------------------------------
-- }}} ToLogStr ---------------------------------------------------------------

-- {{{ LogStr -----------------------------------------------------------------

-- | Construct 'LogStr' from multiple pieces that have instance of 'ToLogStr'
-- type class.
logStr :: LogStrArgs args => args
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
