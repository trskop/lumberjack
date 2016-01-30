{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
    where

import Test.Framework (Test, testGroup)

--import qualified TestCase.Data.LogStr.Class as Class (tests)
--import qualified TestCase.Data.LogStr.Formatting as Formatting (tests)
--import qualified TestCase.Data.LogStr.Formatting.Internal
--  as Formatting.Internal (tests)
import qualified TestCase.Data.LogStr.Internal as Internal (tests)


tests :: [Test]
tests =
    [ testGroup "TestCase.Data.LogStr.Internal" Internal.tests
--  , testGroup "TestCase.Data.LogStr.Class" Class.tests
--  , testGroup "TestCase.Data.LogStr.Formatting.Internal"
--      Formatting.Internal.tests
--  , testGroup "TestCase.Data.LogStr.Formatting" Formatting.tests
    ]
