{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Tests for Data.LogStr.Internal module.
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude, OverloadedStrings
--
-- Tests for "Data.LogStr.Internal" module.
module TestCase.Data.LogStr.Internal (tests)
  where

import Control.Applicative (liftA2)
import Data.Bool (Bool(True), (||), (&&))
import Data.Eq (Eq((/=), (==)))
import Data.Function ((.), ($), id)
import Data.Monoid (Monoid(mappend, mempty))
import Data.Ord (Ord(compare), Ordering(EQ))
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(fromString), String)
import Text.Show (Show(show))

import qualified Data.ByteString as Strict.ByteString (length, null)
import qualified Data.ByteString.Builder as Builder
    ( stringUtf8
    , toLazyByteString
    )

import Data.Default.Class (Default(def))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck.Instances ()

import Data.LogStr.Internal
    ( LogStr
    , empty
    , fromLogStr
    , length
    , null
    , toLogStr
    , toStrictByteString
    )


tests :: [Test]
tests =
    [ testProperty "forall s. length (toLogStr s) = length s"
        $ length . toLogStr <==> Strict.ByteString.length
    , testProperty "forall s. null (toLogStr s) = null s"
        $ null . toLogStr <==> Strict.ByteString.null
    , testGroup "empty"
        [ testCase "length empty = 0" $ length empty @?= 0
        , testCase "fromLogStr empty = \"\"" $ fromLogStr empty @?= ""
        , testCase "null empty = True" $ null empty @?= True
        , testCase "toLogStr \"\" = empty" $ toLogStr "" @?= empty
        ]
    , testGroup "instance Eq LogStr"
        [ testCase "empty == empty" $ (empty == empty) @?= True
        , testProperty "forall s. empty /= toLogStr s <==> s /= \"\""
            $ isNot empty . toLogStr <==> isNot ""
        , testProperty "forall s. s == s <==> length s == length s"
            $ \s ->
                let b = toLogStr s
                in (b == b) && (length b == length b)
        , testProperty "forall s. s == s <==> fromLogStr s == fromLogStr s"
            $ \s ->
                let b = toLogStr s
                in (b == b) && (fromLogStr b == fromLogStr b)
        , testProperty "forall s1 s2. length s1 == length s2\
            \ <==> length (toLogStr s1) == length (toLogStr s2)"
            $ \s1 s2 ->
                let lb1 = length (toLogStr s1)
                    lb2 = length (toLogStr s2)
                    ls1 = Strict.ByteString.length s1
                    ls2 = Strict.ByteString.length s2
                in ((ls1 == ls2) && (lb1 == lb2))
                    || ((ls1 /= ls2) && (lb1 /= lb2))
        , testProperty "forall s1 s2. s1 == s2 <==> toLogStr s1 == toLogStr s2"
            $ \s1 s2 ->
                let b1 = toLogStr s1
                    b2 = toLogStr s2
                in ((b1 == b2) && (s1 == s2)) || ((b1 /= b2) && (s1 /= s2))
        ]
    , testGroup "instance Ord LogStr"
        [ testProperty "forall s1 s2.\
            \ s1 `compare` s2 = toLogStr s1 `compare` toLogStr s2"
            -- Correctnes of (strict) ByteString ordering implies correctnes
            -- of LogStr ordering.
            $ \s1 s2 -> compare s1 s2 == (toLogStr s1 `compare` toLogStr s2)
        , testProperty "forall s. toLogStr s `compare` toLogStr s = EQ"
            $ \s -> let b = toLogStr s in compare b b == EQ
        ]
    , testCase "instance Default LogStr where def = empty" $ def @?= empty
    , testGroup "instance IsString LogStr"
        [ testCase "fromString \"\" = empty" $ fromString "" @?= empty
        , testProperty "fromString generates correct UTF-8 encoded LogStr"
            $ fromLogStr . fromString
                <==> toStrictByteString . Builder.toLazyByteString
                    . Builder.stringUtf8
        , testProperty "forall s. length (fromString s)\
            \ = length (fromLogStr (fromString s)"
            $ (length <==> Strict.ByteString.length . fromLogStr) . fromString
        ]
    , testProperty "instance Show LogStr where show = ..."
        $ show . (fromString :: String -> LogStr)
            <==> show . toStrictByteString . Builder.toLazyByteString
                . Builder.stringUtf8
    , testGroup "instance Monoid LogStr"
        [ testCase "mempty = empty" $ mempty @?= empty
        , testProperty "forall s1 s2.\
            \ toLogStr s1 `mappend` toLogStr s2 = toLogStr (s1 `mappend` s2)"
            $ \s1 s2 ->
                (toLogStr s1 `mappend` toLogStr s2)
                    == toLogStr (s1 `mappend` s2)
        , testProperty "forall s1 s2.\
            \ length (toLogStr s1 `mappend` toLogStr s2)\
            \ = length (toLogStr (s1 `mappend` s2))"
            $ \s1 s2 ->
                length (toLogStr s1 `mappend` toLogStr s2)
                    == length (toLogStr (s1 `mappend` s2))
        , testProperty "forall s. toLogStr s `mappend` mempty = toLogStr s"
            $ ((`mappend` mempty) <==> id) . toLogStr
        , testProperty "forall s. mempty `mappend` toLogStr s = toLogStr s"
            $ (mappend mempty <==> id) . toLogStr
        , testProperty "forall s1 s2 s3.\
            \ toLogStr s1 `mappend` (toLogStr s2 `mappend` toLogStr s3)\
            \ = (toLogStr s1 `mappend` toLogStr s2) `mappend` toLogStr s3"
            $ \s1 s2 s3 ->
                let b1 = toLogStr s1
                    b2 = toLogStr s2
                    b3 = toLogStr s3
                in (b1 `mappend` (b2 `mappend` b3))
                    == ((b1 `mappend` b2) `mappend` b3)
        ]
    , testGroup "instance Semigroup LogStr"
        [ testProperty "forall s1 s2.\
            \ toLogStr s1 <> toLogStr s2 = toLogStr (s1 <> s2)"
            $ \s1 s2 -> (toLogStr s1 <> toLogStr s2) == toLogStr (s1 <> s2)
        , testProperty "forall s1 s2. length (toLogStr s1 <> toLogStr s2)\
            \ = length (toLogStr (s1 <> s2))"
            $ \s1 s2 ->
                length (toLogStr s1 <> toLogStr s2)
                    == length (toLogStr (s1 <> s2))
        , testProperty "forall s1 s2 s3.\
            \ toLogStr s1 <> (toLogStr s2 <> toLogStr s3)\
            \ = (toLogStr s1 <> toLogStr s2) <> toLogStr s3"
            $ \s1 s2 s3 ->
                let b1 = toLogStr s1
                    b2 = toLogStr s2
                    b3 = toLogStr s3
                in (b1 <> (b2 <> b3)) == ((b1 <> b2) <> b3)
        , testProperty "forall s1 s2.\
            \ toLogStr s1 <> toLogStr s2 = toLogStr s1 `mappend` toLogStr s2"
            $ \s1 s2 ->
                let b1 = toLogStr s1
                    b2 = toLogStr s2
                in (b1 <> b2) == (b1 `mappend` b2)
        ]
    ]

(<==>) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(<==>) = liftA2 (==)
infix 8 <==>

isNot :: Eq a => a -> a -> Bool
isNot = (/=)
