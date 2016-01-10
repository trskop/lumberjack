{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015-2016, Peter Trško
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
module Data.LogStr
    (
    -- * LogStr Data Type
      LogStr
    , fromLogStr
    , empty
    , length
    , null

    -- * Conversion To LogStr
    , ToLogStr(..)

    -- ** Hexadecimal Representation
    , Hexadecimal
    , hex

    -- ** Conversion Using Show Instances
    , Showed
    , showed
    , showed1
    , showed2

    -- ** Converting to Space Separated Text
    , words
    , words1
    , words2

    -- * Generic Logging Function
    , LogStrArgs(..)
    , logStr
    )
  where

import Data.Data (Constr)
import Data.Function ((.), ($), id)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Monoid (Monoid(mappend))
import Data.String (String)
import Data.Typeable (Typeable, TypeRep)
import Data.Proxy (Proxy(Proxy))
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Text.Show (Show(show))

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI (original)
import qualified Data.ByteString as Strict (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
    ( int16Dec
    , int16HexFixed
    , int32Dec
    , int32HexFixed
    , int64Dec
    , int64HexFixed
    , int8Dec
    , int8HexFixed
    , intDec
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
import qualified Data.Text as Strict (Text)
--import qualified Data.Text as Strict.Text (pack)
import qualified Data.Text.Encoding as Strict.Text (encodeUtf8)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (pack)
import qualified Data.Text.Lazy.Encoding as Lazy.Text (encodeUtf8)

import qualified Data.ByteString.Base16 as Strict.ByteString.Base16 (encode)
import qualified Data.ByteString.Base16.Lazy as Lazy.ByteString.Base16 (encode)
import Data.NumberLength
    ( BoundedNumberLength(maxNumberLengthHex)
    , NumberLength(numberLength, numberLengthHex)
    , SignedNumberLength(signedNumberLength)
    )
import qualified Data.LogLevel.Common as Common (LogLevel)
import qualified Data.LogLevel.Common as Common.LogLevel (toString)
import qualified Data.LogLevel.Syslog as Syslog (LogLevel)
import qualified Data.LogLevel.Syslog as Syslog.LogLevel (toString)
import Data.Tagged (Tagged(Tagged))

import Data.LogStr.Internal
    ( LogStr
    , empty
    , fromLogStr
    , length
    , null
    )
import qualified Data.LogStr.Internal as Internal
    ( LogStr(LogStr)
    , toLogStr
    , toLogStrWith
    , toStrictByteString
    )


-- {{{ ToLogStr ---------------------------------------------------------------

-- | Type class for converting values in to 'LogStr'.
class ToLogStr msg where
    -- | Convert a message\/value in to 'LogStr'.
    toLogStr :: msg -> LogStr

instance (ToLogStr a, ToLogStr b) => ToLogStr (a, b) where
    toLogStr (a, b) = toLogStr a `mappend` toLogStr b

instance (ToLogStr a, ToLogStr b, ToLogStr c) => ToLogStr (a, b, c) where
    toLogStr (a, b, c) = toLogStr a `mappend` toLogStr b `mappend` toLogStr c

instance
    ( ToLogStr a
    , ToLogStr b
    , ToLogStr c
    , ToLogStr d
    ) => ToLogStr (a, b, c, d)
  where
    toLogStr (a, b, c, d) = toLogStr a `mappend` toLogStr b `mappend` toLogStr c
        `mappend` toLogStr d

instance
    ( ToLogStr a1
    , ToLogStr a2
    , ToLogStr a3
    , ToLogStr a4
    , ToLogStr a5
    ) => ToLogStr (a1, a2, a3, a4, a5)
  where
    toLogStr (a1, a2, a3, a4, a5) =
        toLogStr (a1, a2, a3, a4) `mappend` toLogStr a5

instance
    ( ToLogStr a1
    , ToLogStr a2
    , ToLogStr a3
    , ToLogStr a4
    , ToLogStr a5
    , ToLogStr a6
    ) => ToLogStr (a1, a2, a3, a4, a5, a6)
  where
    toLogStr (a1, a2, a3, a4, a5, a6) =
        toLogStr (a1, a2, a3, a4, a5) `mappend` toLogStr a6

instance
    ( ToLogStr a1
    , ToLogStr a2
    , ToLogStr a3
    , ToLogStr a4
    , ToLogStr a5
    , ToLogStr a6
    , ToLogStr a7
    ) => ToLogStr (a1, a2, a3, a4, a5, a6, a7)
  where
    toLogStr (a1, a2, a3, a4, a5, a6, a7) =
        toLogStr (a1, a2, a3, a4, a5, a6) `mappend` toLogStr a7

instance
    ( ToLogStr a1
    , ToLogStr a2
    , ToLogStr a3
    , ToLogStr a4
    , ToLogStr a5
    , ToLogStr a6
    , ToLogStr a7
    , ToLogStr a8
    ) => ToLogStr (a1, a2, a3, a4, a5, a6, a7, a8)
  where
    toLogStr (a1, a2, a3, a4, a5, a6, a7, a8) =
        toLogStr (a1, a2, a3, a4, a5, a6, a7) `mappend` toLogStr a8

instance
    ( ToLogStr a1
    , ToLogStr a2
    , ToLogStr a3
    , ToLogStr a4
    , ToLogStr a5
    , ToLogStr a6
    , ToLogStr a7
    , ToLogStr a8
    , ToLogStr a9
    ) => ToLogStr (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
    toLogStr (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        toLogStr (a1, a2, a3, a4, a5, a6, a7, a8) `mappend` toLogStr a9

instance ToLogStr Constr where
    toLogStr = toLogStr . show

instance ToLogStr LogStr where
    toLogStr = id
    {-# INLINE toLogStr #-}

instance ToLogStr String where
    toLogStr = toLogStr . Lazy.Text.pack
    {-# INLINEABLE toLogStr #-}

instance ToLogStr TypeRep where
    toLogStr = toLogStr . show

-- {{{ Case Insensitive -------------------------------------------------------

instance (ToLogStr a) => ToLogStr (CI a) where
    toLogStr = toLogStr . CI.original

-- }}} Case Insensitive -------------------------------------------------------

-- {{{ Instances for strict and lazy ByteString and Text ----------------------

instance ToLogStr Strict.ByteString where
    toLogStr = Internal.toLogStr
    {-# INLINEABLE toLogStr #-}

instance ToLogStr Lazy.ByteString where
    toLogStr = Internal.toLogStrWith Internal.toStrictByteString
    {-# INLINEABLE toLogStr #-}

instance ToLogStr Strict.Text where
    toLogStr = Internal.toLogStrWith Strict.Text.encodeUtf8
    {-# INLINEABLE toLogStr #-}

instance ToLogStr Lazy.Text where
    toLogStr = toLogStr . Lazy.Text.encodeUtf8
    {-# INLINEABLE toLogStr #-}

-- }}} Instances for strict and lazy ByteString and Text ----------------------

-- {{{ Instances for Int* and Word* types -------------------------------------

instance ToLogStr Int where
    toLogStr = mkLogStr signedNumberLength Builder.intDec

instance ToLogStr Int8 where
    toLogStr = mkLogStr signedNumberLength Builder.int8Dec

instance ToLogStr Int16 where
    toLogStr = mkLogStr signedNumberLength Builder.int16Dec

instance ToLogStr Int32 where
    toLogStr = mkLogStr signedNumberLength Builder.int32Dec

instance ToLogStr Int64 where
    toLogStr = mkLogStr signedNumberLength Builder.int64Dec

instance ToLogStr Word where
    toLogStr = mkLogStr numberLength Builder.wordDec

instance ToLogStr Word8 where
    toLogStr = mkLogStr numberLength Builder.word8Dec

instance ToLogStr Word16 where
    toLogStr = mkLogStr numberLength Builder.word16Dec

instance ToLogStr Word32 where
    toLogStr = mkLogStr numberLength Builder.word32Dec

instance ToLogStr Word64 where
    toLogStr = mkLogStr numberLength Builder.word64Dec

-- }}} Instances for Int* and Word* types -------------------------------------

-- {{{ LogLevel ---------------------------------------------------------------

instance ToLogStr Syslog.LogLevel where
    toLogStr level = toLogStr (Syslog.LogLevel.toString level :: Strict.Text)
    {-# INLINEABLE toLogStr #-}

instance ToLogStr Common.LogLevel where
    toLogStr level = toLogStr (Common.LogLevel.toString level :: Strict.Text)
    {-# INLINEABLE toLogStr #-}

-- }}} LogLevel ---------------------------------------------------------------

-- {{{ Hexadecimal ------------------------------------------------------------

data Hexadecimal
  deriving (Generic, Typeable)

-- | Mark value as 'Hexadecimal' when converting it to 'LogStr'.
hex :: a -> Tagged Hexadecimal a
hex = Tagged
{-# INLINE hex #-}

instance ToLogStr (Tagged Hexadecimal Strict.ByteString) where
    toLogStr (Tagged bs) = toLogStr $ Strict.ByteString.Base16.encode bs

instance ToLogStr (Tagged Hexadecimal Lazy.ByteString) where
    toLogStr (Tagged bs) = toLogStr $ Lazy.ByteString.Base16.encode bs

-- {{{ Instances for Int* and Word* types -------------------------------------

{- TODO: Find elegant way how to get around not having Builder.intHexFixed
instance ToLogStr (Tagged Hexadecimal Int) where
    toLogStr n = Internal.LogStr (signedNumberLength n) (Builder.intDec n)
-}

instance ToLogStr (Tagged Hexadecimal Int8) where
    toLogStr = mkLogStrT (maxNumberLengthHex . proxyOf) Builder.int8HexFixed

instance ToLogStr (Tagged Hexadecimal Int16) where
    toLogStr = mkLogStrT (maxNumberLengthHex . proxyOf) Builder.int16HexFixed

instance ToLogStr (Tagged Hexadecimal Int32) where
    toLogStr = mkLogStrT (maxNumberLengthHex . proxyOf) Builder.int32HexFixed

instance ToLogStr (Tagged Hexadecimal Int64) where
    toLogStr = mkLogStrT (maxNumberLengthHex . proxyOf) Builder.int64HexFixed

instance ToLogStr (Tagged Hexadecimal Word) where
    toLogStr = mkLogStrT numberLengthHex Builder.wordHex

instance ToLogStr (Tagged Hexadecimal Word8) where
    toLogStr = mkLogStrT numberLengthHex Builder.word8Hex

instance ToLogStr (Tagged Hexadecimal Word16) where
    toLogStr = mkLogStrT numberLengthHex Builder.word16Hex

instance ToLogStr (Tagged Hexadecimal Word32) where
    toLogStr = mkLogStrT numberLengthHex Builder.word32Hex

instance ToLogStr (Tagged Hexadecimal Word64) where
    toLogStr = mkLogStrT numberLengthHex Builder.word64Hex

-- }}} Instances for Int* and Word* types -------------------------------------
-- }}} Hexadecimal ------------------------------------------------------------

-- {{{ Showed -----------------------------------------------------------------

data Showed
  deriving (Generic, Typeable)

showed :: a -> Tagged Showed a
showed = Tagged
{-# INLINE showed #-}

showed1 :: (a -> b) -> a -> Tagged Showed b
showed1 = (showed .)
{-# INLINE showed1 #-}

showed2 :: (a -> b -> c) -> a -> b -> Tagged Showed c
showed2 = (showed1 .)
{-# INLINE showed2 #-}

instance Show a => ToLogStr (Tagged Showed a) where
    toLogStr (Tagged a) = toLogStr $ show a
    {-# INLINEABLE toLogStr #-}

-- }}} Showed -----------------------------------------------------------------

-- {{{ Words ------------------------------------------------------------------

data Words
  deriving (Generic, Typeable)

words :: a -> Tagged Words a
words = Tagged
{-# INLINE words #-}

words1 :: (a -> b) -> a -> Tagged Words b
words1 = (words .)
{-# INLINE words1 #-}

words2 :: (a -> b -> c) -> a -> b -> Tagged Words c
words2 = (words1 .)
{-# INLINE words2 #-}

--instance (Foldable f, ToLogStr a) => ToLogStr (Tagged Words (f a)) where
--    toLogStr (Tagged as) = ...
--    {-# INLINEABLE toLogStr #-}

-- }}} Words ------------------------------------------------------------------
-- }}} ToLogStr ---------------------------------------------------------------

-- {{{ Generic Logging Function -----------------------------------------------

-- | Construct a log message from multiple pieces that have instance of
-- 'ToLogStr' type class.
logStr :: LogStrArgs args => args
logStr = logStrArgs empty
{-# INLINE logStr #-}

-- | Class describes variadic arguments of 'log' function.
class LogStrArgs a where
    type Result a

    logStrArgs :: LogStr -> a

instance LogStrArgs LogStr where
    type Result LogStr = LogStr

    logStrArgs = id
    {-# INLINE logStrArgs #-}

instance (ToLogStr a, LogStrArgs r) => LogStrArgs (a -> r) where
    type Result (a -> r) = r

    logStrArgs str a = logStrArgs (str `mappend` toLogStr a)
    {-# INLINEABLE logStrArgs #-}

-- }}} Generic Logging Function -----------------------------------------------

-- {{{ Utility Functions ------------------------------------------------------

mkLogStr
    :: (a -> Int)
    -> (a -> Builder)
    -> a
    -> LogStr
mkLogStr len toBuilder a = Internal.LogStr (len a) (toBuilder a)
{-# INLINE mkLogStr #-}

mkLogStrT
    :: (a -> Int)
    -> (a -> Builder)
    -> Tagged t a
    -> LogStr
mkLogStrT len toBuilder (Tagged a) = mkLogStr len toBuilder a
{-# INLINE mkLogStrT #-}

proxyOf :: a -> Proxy a
proxyOf _ = Proxy
{-# INLINE proxyOf #-}

--newtype Cons f a = Cons {unCons :: a (f a)}
--
--instance Functor f => Cons f where
--    fmap f (Cons a as) = Cons (f a) (fmap f as)
--
--newtype Snoc f a = Snoc {unSnoc :: (f a) a}
--
--instance Functor f => Snoc f where
--    fmap f (Snoc as a) = Snoc (fmap f as) (f a)

-- }}} Utility Functions ------------------------------------------------------
