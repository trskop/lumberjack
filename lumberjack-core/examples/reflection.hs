{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main)
  where

import Data.Proxy (Proxy(Proxy))

import Data.Default.Class (Default(def))
    -- <https://hackage.haskell.org/package/data-default-class>
import Data.Functor.Trans.Tagged (TaggedT(TagT), proxyT)
    -- <https://hackage.haskell.org/package/tagged-transformer>
import Data.Reflection (Reifies(reflect), reify)
    -- <https://hackage.haskell.org/package/reflection>
import System.Lumberjack.Backend
    ( LoggingBackend(pushLogStrLn)
    , SomeLoggingBackend
    , withSomeLoggingBackendM
    )
import System.Lumberjack.FastLogger (fastLogger)


doSomething :: forall s. Reifies s SomeLoggingBackend => TaggedT s IO ()
doSomething = TagT $ do
    -- -->8--
    pushLogStrLn loggingBackend "Some log message."
    -- -->8--
    return ()
  where
    loggingBackend = reflect (Proxy :: Proxy s)

main :: IO ()
main = withSomeLoggingBackendM (fastLogger def) $ \loggingBackend ->
    reify loggingBackend (proxyT doSomething)
