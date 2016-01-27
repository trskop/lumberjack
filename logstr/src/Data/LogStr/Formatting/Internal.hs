{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  HoleyMonoid specialized for LogStr builder.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               NoImplicitPrelude, TypeFamilies
--
-- 'HoleyMonoid' specialized for 'LogStr' builder.
--
-- Inspired by <https://hackage.haskell.org/package/formatting formatting>
-- package. Kept the same naming conventions to make drop-in replecement
-- possible.
module Data.LogStr.Formatting.Internal
  where

import Control.Category (Category((.), id))
import Data.Function (($))
import Data.Monoid (Monoid(mappend, mempty))
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)

import Data.Default.Class (Default(def))
import Data.LogStr.Internal (LogStr)

import Data.HoleyMonoid (HoleyMonoid(HoleyMonoid))
import qualified Data.HoleyMonoid as HoleyMonoid (bind, map, now)


-- | Represents a log message formatter.
--
-- Type @r@ represents the final retur value; and type @a@ is an accumulator
-- that is used to build up arguments passed to the formatter. In example:
--
-- @
-- int :: 'Format' r (Text -> r)
-- char :: 'Format' r (Char -> r)
-- int '%' char :: 'Format' r (Int -> (Char -> r))
-- @
--
-- By unwrapping last expression we get:
--
-- @
-- 'runFormat' $ int '%' char :: ('LogStr' -> r) -> (Int -> (Char -> r))
-- @
--
-- Since the @->@ type operator is right associative, we can transform above
-- in to identical expression, but without unnecessary brackets:
--
-- @
-- 'runFormat' $ int '%' char :: ('LogStr' -> r) -> Int -> Char -> r
-- @
--
-- By passing 'id' as a first argiment, we get:
--
-- @
-- 'runFormat' (int '%' char) id :: Int -> Char -> 'LogStr'
-- @
--
-- That is the whole trick behind 'Format' type. j
newtype Format r a = Format {toHoleyMonoid :: HoleyMonoid LogStr r a}
  deriving (Generic, Generic1, Typeable)

instance Default (Format r r) where
    def = id

instance Default (Format r (a -> r)) where
    def = mempty

instance Monoid (Format r (a -> r)) where
    mempty = Format . HoleyMonoid $ \f _ -> f mempty
    Format (HoleyMonoid f1) `mappend` Format (HoleyMonoid f2) =
        Format . HoleyMonoid $ \f a ->
            f1 (\b1 -> f2 (\b2 -> f (b1 `mappend` b2)) a) a

instance Semigroup (Format r (a -> r)) where
    (<>) = mappend

-- | Writting @\"some text\"@ is the same as writting @'now' \"some text\"@,
-- when using /OverloadedStrings/ language extension.
instance (a ~ r) => IsString (Format r a) where
    fromString = now . fromString

instance Category Format where
    id = Format id
    (.) = (%)

-- | Unwrap 'Format' type in to its underlying representation. See also 'run'
-- and 'format'.
runFormat :: Format r a -> (LogStr -> r) -> a
runFormat (Format (HoleyMonoid f)) = f

-- | Evaluate 'Format' in to its accumulator type. See 'format' for more
-- information.
run :: Format LogStr a -> a
run (Format (HoleyMonoid f)) = f id

-- | Insert a constant value of 'LogStr'.
--
-- >>> run $ now (toLogStr "te") % now (toLogStr "xt")
-- "text"
--
-- With /OverloadedStrings/ one can write:
--
-- @
-- 'run' $ 'now' \"te\" % 'now' \"xt\"
-- @
--
-- Or even simpler:
--
-- @
-- 'run' $ \"te\" % \"xt\"
-- @
--
-- See 'Format' type for more details, especially its 'IsString' instance.
now :: LogStr -> Format r r
now = Format . HoleyMonoid.now

-- | Compose two formatters. One can think of it as a @%@ operator from
-- @printf@ known from other languages.
--
-- This is function si the same as specialized '.' from "Control.Category",
-- which unfortunately collides with standard function composition operator.
(%) :: Format r a -> Format r' r -> Format r' a
Format f % Format g = Format (f . g)
infixr 9 %

bind :: Format r a -> (LogStr -> Format r' r) -> Format r' a
Format f `bind` g = Format $ f `HoleyMonoid.bind` (toHoleyMonoid . g)

later :: (a -> LogStr) -> Format r (a -> r)
later f = Format $ HoleyMonoid (. f)

-- | Modify underlying 'LogStr' builder.
map :: (LogStr -> LogStr) -> Format r a -> Format r a
map f (Format m) = Format $ HoleyMonoid.map f m
