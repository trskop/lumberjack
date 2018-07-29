{-# LANGUAGE OverloadedStrings #-}
module Main (main)
  where

import Control.Monad.Logger (LoggingT, pushLogLn, runLoggingT)
import Data.Default.Class (Default(def))
    -- <https://hackage.haskell.org/package/data-default-class>
import System.Lumberjack.Backend (withSomeLoggingBackendM)
import System.Lumberjack.FastLogger (fastLogger)


doSomething :: LoggingT IO ()
doSomething = do
    -- -->8--
    pushLogLn "Some log message."
    -- -->8--
    pure ()

main :: IO ()
main = withSomeLoggingBackendM (fastLogger def) $ runLoggingT doSomething
