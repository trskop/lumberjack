{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Main
-- Description:  Tests main
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- Tests main.
module Main (main)
    where

import System.IO (IO)

import Test.Framework (defaultMain)

import TestCase (tests)


main :: IO ()
main = defaultMain tests
