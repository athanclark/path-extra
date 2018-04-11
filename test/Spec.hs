{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , ScopedTypeVariables
  #-}

module Main where

import Path (parseAbsDir, parseAbsFile, Path, Abs, Dir, File)
import Path.Extended (Location (..), QueryParam, locationAbsDirParser, locationAbsFileParser)

import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Attoparsec.Text (parseOnly)
import Control.Exception (SomeException)
import Control.Monad (replicateM)

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck (Arbitrary (..), suchThat)
import Test.QuickCheck.Gen (Gen, choose, elements, listOf)
import Test.QuickCheck.Instances ()



newtype ArbitraryLocationAbsDir = ArbitraryLocationAbsDir
  { getArbitraryLocationAbsDir :: Location Abs Dir
  } deriving (Eq, Show)

instance Arbitrary ArbitraryLocationAbsDir where
  arbitrary = do
    locPath <- arbitraryPath
    locQueryParams <- arbitraryQueryParams
    locFragment <- arbitraryFragment
    pure $ ArbitraryLocationAbsDir Location
      { locParentJumps = 0
      , locPath
      , locFileExt = Nothing
      , locQueryParams
      , locFragment
      }
    where
      arbitraryFragment :: Gen (Maybe String)
      arbitraryFragment = do
        build <- arbitrary
        if not build
          then pure Nothing
          else
            fmap Just $ listOf (elements (['A'..'Z'] <> ['a'..'z'])) `suchThat` (not . null)

      arbitraryQueryParams :: Gen [QueryParam]
      arbitraryQueryParams = do
        build <- arbitrary
        if not build
          then pure []
          else do
            let arbitraryVal :: Gen String
                arbitraryVal =
                  listOf (elements (['A'..'Z'] <> ['a'..'z'])) `suchThat` (not . null)
            n <- choose (1,4)
            let arbitraryKV :: Gen QueryParam
                arbitraryKV = do
                  k <- arbitraryVal
                  buildVal <- arbitrary
                  mV <-
                    if buildVal
                    then Just <$> arbitraryVal
                    else pure Nothing
                  pure (k,mV)
            replicateM n arbitraryKV

      arbitraryPath :: Gen (Path Abs Dir)
      arbitraryPath = do
        n <- choose (0,5)
        xs <- replicateM n arbitraryChunk
        pure $ case parseAbsDir ( case xs of
                                    [] -> "/"
                                    _ -> T.unpack ("/" <> T.intercalate "/" xs <> "/")
                                ) of
                 Left (e :: SomeException) ->
                   error $ "Can't parse abs dir! " <> show e
                 Right path -> path

      arbitraryChunk :: Gen T.Text
      arbitraryChunk = fmap T.pack $
        listOf (elements (['A'..'Z'] <> ['a'..'z'])) `suchThat` (not . null)


newtype ArbitraryLocationAbsFile = ArbitraryLocationAbsFile
  { getArbitraryLocationAbsFile :: Location Abs File
  } deriving (Eq, Show)

instance Arbitrary ArbitraryLocationAbsFile where
  arbitrary = do
    locPath <- arbitraryPath
    locFileExt <- arbitraryFileExt
    locQueryParams <- arbitraryQueryParams
    locFragment <- arbitraryFragment
    pure $ ArbitraryLocationAbsFile Location
      { locParentJumps = 0
      , locPath
      , locFileExt
      , locQueryParams
      , locFragment
      }
    where
      arbitraryFragment :: Gen (Maybe String)
      arbitraryFragment = do
        build <- arbitrary
        if not build
          then pure Nothing
          else
            fmap Just $ listOf (elements (['A'..'Z'] <> ['a'..'z'])) `suchThat` (not . null)

      arbitraryQueryParams :: Gen [QueryParam]
      arbitraryQueryParams = do
        build <- arbitrary
        if not build
          then pure []
          else do
            let arbitraryVal :: Gen String
                arbitraryVal =
                  listOf (elements (['A'..'Z'] <> ['a'..'z'])) `suchThat` (not . null)
            n <- choose (1,4)
            let arbitraryKV :: Gen QueryParam
                arbitraryKV = do
                  k <- arbitraryVal
                  buildVal <- arbitrary
                  mV <-
                    if buildVal
                    then Just <$> arbitraryVal
                    else pure Nothing
                  pure (k,mV)
            replicateM n arbitraryKV

      arbitraryPath :: Gen (Path Abs File)
      arbitraryPath = do
        n <- choose (1,5)
        xs <- replicateM n arbitraryChunk
        pure $ case parseAbsFile ( case xs of
                                    [] -> "/"
                                    _ -> T.unpack ("/" <> T.intercalate "/" xs)
                                 ) of
                 Left (e :: SomeException) ->
                   error $ "Can't parse abs file! " <> show e
                 Right path -> path

      arbitraryFileExt :: Gen (Maybe String)
      arbitraryFileExt = do
        build <- arbitrary
        if not build
          then pure Nothing
          else
            Just <$> listOf (elements (['A'..'Z'] <> ['a'..'z'])) `suchThat` (not . null)

      arbitraryChunk :: Gen T.Text
      arbitraryChunk = fmap T.pack $
        listOf (elements (['A'..'Z'] <> ['a'..'z'])) `suchThat` (not . null)



printParseIsoAbsDir :: ArbitraryLocationAbsDir -> Bool
printParseIsoAbsDir (ArbitraryLocationAbsDir loc) =
  Right loc == parseOnly locationAbsDirParser (T.pack (show loc))

printParseIsoAbsFile :: ArbitraryLocationAbsFile -> Bool
printParseIsoAbsFile (ArbitraryLocationAbsFile loc) =
  Right loc == parseOnly locationAbsFileParser (T.pack (show loc))



main :: IO ()
main = defaultMain $
  testGroup "Path.Extended"
    [ QC.testProperty "Print / Parse AbsDir Iso" printParseIsoAbsDir
    , QC.testProperty "Print / Parse AbsFile Iso" printParseIsoAbsFile
    ]
