{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP, DeriveDataTypeable, DeriveFunctor, DeriveGeneric,
--               NoImplicitPrelude, OverloadedStrings, RecordWildCards,
--               TemplateHaskell
--
-- TODO
module System.Lumberjack.Location
  where

import Prelude
    ( fromIntegral
#if !MIN_VERSION_template_haskell(2,10,0)
    -- Since "instance Lift Word" is available since template-haskell
    -- >=2.10.0.0, it is necessary to go throug some available compatible
    -- numeric instance.
    , Integral(toInteger)
    , Num(fromInteger)
#endif
    )

import Data.Bool (otherwise)
import Data.Data (Data, Typeable)
import Data.Eq (Eq((==)))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Ord (Ord((<)))
import Data.String (IsString, String)
import Data.Tuple (fst, snd)
import Data.Word (Word)
import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (CharPos, Lift(lift), Loc(..))

import Data.Default.Class (Default(def))
import Data.Function.Between.Strict ((~@@^>))

import System.Lumberjack.LogStr (LogStr, ToLogStr(toLogStr), logStr)


-- | Represents location in a Haskell source code file which is part of a
-- specific Haskell package.
data Location str = Location
    { _fileName :: str
    -- ^ See 'fileName' for details.
    , _packageName :: str
    -- ^ See 'packageName' for details.
    , _moduleName :: str
    -- ^ See 'moduleName' for details.
    , _startLine :: !Word
    -- ^ See 'startLine' for details.
    , _startChar :: !Word
    -- ^ See 'startChar' for details.
    , _endLine :: !Word
    -- ^ See 'endLine' for details.
    , _endChar :: !Word
    -- ^ See 'endChar' for details.
    }
  deriving (Data, Eq, Functor, Generic, Ord, Typeable)

-- | File name of a Haskell source code file in which Haskell module
-- 'moduleName' is located and that is part of 'packageName'.
fileName :: Functor f => (s -> f s) -> Location s -> f (Location s)
fileName = _fileName ~@@^> \s b -> s{_fileName = b}

-- | Haskell package name of which 'fileName' and 'moduleName' are part of.
packageName :: Functor f => (s -> f s) -> Location s -> f (Location s)
packageName = _packageName ~@@^> \s b -> s{_packageName = b}

-- | Haskell module name which is located in 'fileName' and is part of
-- Haskell package 'packageName'.
moduleName :: Functor f => (s -> f s) -> Location s -> f (Location s)
moduleName = _moduleName ~@@^> \s b -> s{_moduleName = b}

-- | Start line of a line range inside Haskell source code file 'fileName'.
startLine :: Functor f => (Word -> f Word) -> Location s -> f (Location s)
startLine = _startLine ~@@^> \s b -> s{_startLine = b}

-- | Character position on 'startLine'.
startChar :: Functor f => (Word -> f Word) -> Location s -> f (Location s)
startChar = _startChar ~@@^> \s b -> s{_startChar = b}

-- | End line of a line range inside Haskell source code file 'fileName'.
endLine :: Functor f => (Word -> f Word) -> Location s -> f (Location s)
endLine = _endLine ~@@^> \s b -> s{_endLine = b}

-- | Character position on 'endLine'.
endChar :: Functor f => (Word -> f Word) -> Location s -> f (Location s)
endChar = _endChar ~@@^> \s b -> s{_endChar = b}

instance Lift str => Lift (Location str) where
    lift Location{..} = [|Location
        { _fileName = $(lift _fileName)
        , _packageName = $(lift _packageName)
        , _moduleName = $(lift _moduleName)

        -- "instance Lift Word" is available since template-haskell >=2.10.0.0.
#if MIN_VERSION_template_haskell(2,10,0)
        , _startLine = $(lift _startLine)
        , _startChar = $(lift _startChar)
        , _endLine = $(lift _endLine)
        , _endChar = $(lift _endChar)
#else
        , _startLine = fromInteger $(lift (toInteger _startLine))
        , _startChar = fromInteger $(lift (toInteger _startChar))
        , _endLine = fromInteger $(lift (toInteger _endLine))
        , _endChar = fromInteger $(lift (toInteger _endChar))
#endif
        }|]

-- | Represents unknown 'Location':
--
-- @
-- 'fileName' = \"\<unknown\>\"
-- 'packageName' = \"\<unknown\>\"
-- 'moduleName' = \"\<unknown\>\"
-- 'startLine' = 0
-- 'startChar' = 0
-- 'endLine' = 0
-- 'endChar' = 0
-- @
instance IsString str => Default (Location str) where
    def = Location
        { _fileName = unknown
        , _packageName = unknown
        , _moduleName = unknown
        , _startLine = unknownPosition
        , _startChar = unknownPosition
        , _endLine = unknownPosition
        , _endChar = unknownPosition
        }
      where
        unknown = "<unknown>"
        unknownPosition = 0

-- | Serialized in to: @\<package\>:\<module\> \<file\>:\<line\>:\<char\>@.
instance (ToLogStr str) => ToLogStr (Location str) where
    toLogStr Location{..} = logStr _packageName colon _moduleName space
        _fileName colon _startLine colon _startChar
      where
        colon = ":" :: LogStr
        space = " " :: LogStr

-- | Serializes 'Location' in to:
--
-- @
-- \@(\<package\>:\<module\> \<file\>:\<line\>:\<char\>)
-- @
--
-- Same interpretation is used by /monad-logger package/.
monadLoggerStyleLocation
    :: (Eq str, IsString str, ToLogStr str)
    => Location str
    -> LogStr
monadLoggerStyleLocation loc
  | loc == def = mempty
  | otherwise  = "@(" <> toLogStr loc <> ")"

-- | Simple location as is in example produced by converting 'Loc' in to
-- 'Location'. See also 'locationFromLoc'.
type Location' = Location String

-- | Convert 'Loc' data type from
-- <https://hackage.haskell.org/package/template-haskell template-haskell> in
-- to 'Location'.
locationFromLoc :: Loc -> Location'
locationFromLoc Loc{..} = Location
    { _fileName = loc_filename
    , _packageName = loc_package
    , _moduleName = loc_module
    , _startLine = fromCharPos fst loc_start
    , _startChar = fromCharPos snd loc_start
    , _endLine = fromCharPos fst loc_end
    , _endChar = fromCharPos snd loc_end
    }
  where
    fromCharPos :: (CharPos -> Int) -> CharPos -> Word
    fromCharPos f charPos
      | n < 0     = 0
      | otherwise = fromIntegral n
      where n = f charPos
