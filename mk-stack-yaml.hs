#!/usr/bin/env stack
-- stack --resolver lts-11.15 script
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((>>=))
import Data.Foldable (forM_)
import Data.Function (($), (.))
import Data.Word (Word)
import System.IO (IO)

import Turtle


ltsVersions :: [(Word, Word)]
ltsVersions =
    [ ( 9, 21) -- Final LTS 9 version
    , (10, 10) -- Final LTS 10 version
    , (11, 15)
    ]

stackYamlTemplate :: FilePath
stackYamlTemplate = fromText "stack-template.yaml"

main :: IO ()
main = forM_ ltsVersions $ \v ->
    output (mkStackYamlFile v)
        $ sedPrefix (setResolver v) (input stackYamlTemplate)
  where
    setResolver v = prefix $ (<> mkLtsVersionStr v) <$> "resolver: "

    mkStackYamlFile (major, _) =
        fromText $ format ("stack-lts-" % u % ".yaml") major

    mkLtsVersionStr (major, minor) = format ("lts-" % u % "." % u) major minor
