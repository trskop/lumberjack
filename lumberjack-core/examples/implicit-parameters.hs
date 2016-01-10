{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main)
  where

import Data.Default.Class (Default(def))
    -- <https://hackage.haskell.org/package/data-default-class>
import System.Lumberjack.Backend
    ( LoggingBackend(pushLogStrLn)
    , SomeLoggingBackend
    , withSomeLoggingBackendM
    )
import System.Lumberjack.FastLogger (fastLogger)


doSomething :: (?loggingBackend :: SomeLoggingBackend) => IO ()
doSomething = do
    -- -->8--
    pushLogStrLn ?loggingBackend "Some log message."
    -- -->8--
    return ()

main :: IO ()
main = withSomeLoggingBackendM (fastLogger def) $ \loggingBackend ->
    let ?loggingBackend = loggingBackend in doSomething
