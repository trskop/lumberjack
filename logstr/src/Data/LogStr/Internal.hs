{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Logging message/string builder with O(1) length operation.
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP, FlexibleInstances, DeriveDataTypeable, NoImplicitPrelude,
--               TypeFamilies
--
-- Logging message\/string builder ('LogStr') with O(1) length operation. This
-- package is intended for library writers and not for general usage.
--
-- Based on code from:
-- <https://hackage.haskell.org/package/fast-logger fast-logger> created by
-- Kazu Yamamoto under
-- <https://github.com/kazu-yamamoto/logger/blob/master/fast-logger/LICENSE BSD3 license>.
module Data.LogStr.Internal
    (
    -- * LogStr Data Type
      LogStr(..)

    -- ** Construction
    , empty
    , toLogStr
    , toLogStrWith

    -- ** Conversion To (Strict) ByteString
    , fromLogStr

    -- ** Other Operations
    , length
    , null

    -- * Utility Functions
    , toStrictByteString
    )
  where

import Prelude (Num((+)))

import Data.Bool (Bool)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), on)
import Data.Int (Int)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Ord (Ord(compare))
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show(show))

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
    , toLazyByteString
    )
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString
#if MIN_VERSION_bytestring(0,10,0)
    ( toStrict
#else
    ( toChunks
#endif
    )
import qualified Data.Text.Lazy as Lazy.Text (pack)
import qualified Data.Text.Lazy.Encoding as Lazy.Text (encodeUtf8)

import Data.Default.Class (Default(def))


-- | Log message builder. Monoid operations (including concatenation) are in
-- O(1) and so is 'length'.
data LogStr = LogStr !Int Builder
  deriving (Generic, Typeable)

-- | @'def' = 'empty'@
instance Default LogStr where
    def = empty
    {-# INLINE def #-}

-- | Converts both 'LogStr' values in to (strict) 'Strict.ByteString' before
-- comparison.
instance Eq LogStr where
    (==) = (==) `on` fromLogStr

instance IsString LogStr where
    fromString = toLogStrWith
        $ toStrictByteString . Lazy.Text.encodeUtf8 . Lazy.Text.pack
    {-# INLINEABLE fromString #-}

-- | Monoid axioms hold:
--
-- @
-- forall (s :: 'LogStr').
--   'mempty' ``mappend`` s = s ``mappend`` 'mempty' = s
-- @
--
-- @
-- forall (s1 :: 'LogStr') (s2 :: 'LogStr') (s3 :: 'LogStr').
--   ('s1' ``mappend`` s2) ``mappend`` s3 = 's1' ``mappend`` (s2 ``mappend`` s3)
-- @
--
-- Additional rules that hold:
--
-- @
-- 'mempty' = 'empty'
-- @
--
-- @
-- 'length' 'mempty' = 0
-- @
--
-- @
-- forall (s1 :: 'LogStr') (s2 :: 'LogStr').
--   'length' (s1 ``mappend`` s2) = 'length' s1 '+' 'length' s2
-- @
--
-- @
-- forall (s1 :: 'LogStr') (s2 :: 'LogStr').
--   'fromLogStr' (s1 ``mappend`` s2) = 'fromLogStr' s1 ``mappend`` 'fromLogStr' s2
-- @
instance Monoid LogStr where
    mempty = empty
    {-# INLINE mempty #-}

    LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 `mappend` b2)
    {-# INLINEABLE mappend #-}

-- | Converts both 'LogStr' values in to (strict) 'Strict.ByteString' before
-- comparison.
instance Ord LogStr where
    compare = compare `on` fromLogStr

instance Semigroup LogStr where
    (<>) = mappend

instance Show LogStr where
    show = show . fromLogStr

-- | Obtaining the length of 'LogStr' in O(1).
--
-- @
-- 'length' 'mempty' = 'length' 'empty' = 'length' ('fromString' \"\") = 0
-- @
--
-- @
-- forall logStr.
--     'length' logStr = 'Strict.ByteString.length' 'fromLogStr' logStr
-- @
length :: LogStr -> Int
length (LogStr n _) = n
{-# INLINE length #-}

-- | Check if 'LogStr' is empty (has zero length) in O(1).
--
-- @
-- forall str. 'null' str = True <=> 'length' str = 0 <=> str = 'empty'
-- @
null :: LogStr -> Bool
null = (== 0) . length
{-# INLINE null #-}

-- | Empty 'LogStr', i.e. string with zero length. It is useful in cases when
-- polymorphic 'def' or 'mempty' can not be used without type annotation.
--
-- @
-- 'length' 'empty' = 0
-- 'fromLogStr' 'empty' = 'Strict.ByteString.empty'
-- 'empty' = 'fromString' \"\"
-- @
empty :: LogStr
empty = LogStr 0 (Builder.byteString Strict.ByteString.empty)
{-# INLINE empty #-}

-- | Convert 'LogStr' to UTF-8 encoded (strict) 'ByteString'.
fromLogStr :: LogStr -> Strict.ByteString
fromLogStr (LogStr _ builder) = fromBuilder builder
  where
    fromBuilder = toStrictByteString . Builder.toLazyByteString
{-# INLINEABLE fromLogStr #-}

-- | Create 'LogStr' from UTF-8 encoded (strict) 'Strict.ByteString'.
--
-- @
-- forall str.
--   'fromLogStr' ('toLogStr' str) = str
--   'length' ('toLogStr' str) = 'Strict.ByteString.length' str
-- @
toLogStr :: Strict.ByteString -> LogStr
toLogStr bs = LogStr (Strict.ByteString.length bs) (Builder.byteString bs)
{-# INLINE toLogStr #-}

-- | Create 'LogStr' using conversion function. See also 'toLogStr'.
-- Implemented as:
--
-- @
-- \\f a -> 'toLogStr' (f a)
-- @
toLogStrWith :: (a -> Strict.ByteString) -> a -> LogStr
toLogStrWith = (toLogStr .)
{-# INLINE toLogStrWith #-}

-- | Convert (lazy) 'Lazy.ByteString' to (strict) 'Strict.ByteString'. This
-- function provides compatibility wrapper for various versions of /bytestring/
-- package.
toStrictByteString :: Lazy.ByteString -> Strict.ByteString
toStrictByteString =
#if MIN_VERSION_bytestring(0,10,0)
    Lazy.ByteString.toStrict
#else
    Strict.ByteString.concat . Lazy.ByteString.toChunks
#endif
{-# INLINE toStrictByteString #-}
