{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |
-- Module: Main
-- Copyright: Copyright © 2016 Soostone, Inc.
-- License: BSD3
-- Maintainer: Michael Xavier <michael.xavier@soostone.com>
-- Stability: experimental
--
-- Tests for Haskell AWS S3 bindings
--
module Main
    ( main
    ) where

import           Control.Applicative
import Data.ByteString (ByteString)
import           Control.Arrow                (second)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.List                    as L
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Typeable
import           Data.Proxy
import           Network.HTTP.Client          (HttpException (..),
                                               RequestBody (..), newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Types.Status
import           System.Environment
import           System.Exit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options

import           Aws
import           Aws.S3


newtype BucketOption = BucketOption Bucket
                     deriving (Show, Eq, Ord, Typeable)

instance IsOption BucketOption where
  defaultValue = error "The --bucket flag is required"
  parseValue = Just . BucketOption . T.pack
  optionName = return "bucket"
  optionHelp = return "Bucket to use for performing S3 operations. Tests will write to the 's3-test-object' key."


main :: IO ()
main = do
    args <- getArgs
    runMain args $ map (second tail . span (/= '=')) args
  where
    runMain :: [String] -> [(String,String)] -> IO ()
    runMain args _argsMap
        | any (`elem` helpArgs) args = defaultMainWithIngredients ings tests
        | "--run-with-aws-credentials" `elem` args =
            withArgs (tastyArgs args) . defaultMainWithIngredients ings $ tests
        | otherwise = putStrLn help >> exitFailure
    helpArgs = ["--help", "-h"]
    mainArgs =
        [ "--run-with-aws-credentials"
        ]
    tastyArgs args = flip filter args $ \x -> not
        $ any (`L.isPrefixOf` x) mainArgs
    ings = includingOptions [Option (Proxy :: Proxy BucketOption)]:defaultIngredients


help :: String
help = L.intercalate "\n"
    [ ""
    , "NOTE"
    , ""
    , "This test suite accesses the AWS account that is associated with"
    , "the default credentials from the credential file ~/.aws-keys."
    , ""
    , "By running the tests in this test-suite costs for usage of AWS"
    , "services may incur."
    , ""
    , "In order to actually excute the tests in this test-suite you must"
    , "provide the command line options:"
    , ""
    , "    --run-with-aws-credentials"
    , ""
    , "When running this test-suite through cabal you may use the following"
    , "command:"
    , ""
    , "    cabal test --test-option=--run-with-aws-credentials s3-tests"
    , ""
    ]


tests :: TestTree
tests = testGroup "S3 Tests"
    [ test_head
    , test_get
    ]


-------------------------------------------------------------------------------
-- HeadObject Tests
-------------------------------------------------------------------------------


test_head :: TestTree
test_head = askOption $ \(BucketOption bucket) -> testGroup "HeadObject"
  [ test_head_caching bucket
  ]


test_head_caching :: Bucket -> TestTree
test_head_caching bucket = withResource mkSetup teardown $ \setup -> testGroup "Caches"
  [ testCase "If-Matches match succeeds" $ do
      (cfg, s3cfg, mgr) <- setup
      void (runResourceT (pureAws cfg s3cfg mgr (headObject bucket k) { hoIfMatch = Just payloadMD5 }))
  , testCase "If-Matches mismatch fails with 412" $ do
      (cfg, s3cfg, mgr) <- setup
      assertStatus 412 (runResourceT (pureAws cfg s3cfg mgr (headObject bucket k) { hoIfMatch = Just (T.reverse payloadMD5) }))
  , testCase "If-None-Match mismatch succeeds" $ do
      (cfg, s3cfg, mgr) <- setup
      void (runResourceT (pureAws cfg s3cfg mgr (headObject bucket k) { hoIfNoneMatch = Just (T.reverse payloadMD5) }))
  , testCase "If-None-Match match fails with 304" $ do
      (cfg, s3cfg, mgr) <- setup
      assertStatus 304 (runResourceT (pureAws cfg s3cfg mgr (headObject bucket k) { hoIfNoneMatch = Just payloadMD5 }))
  ]
  where
    k = "s3-test-object"
    content = "example"
    payloadMD5 = "1a79a4d60de6718e8e5b326e338ae533"
    mkSetup = do
      cfg <- baseConfiguration
      let s3cfg = defServiceConfig
      mgr <- newManager tlsManagerSettings
      void (runResourceT (pureAws cfg s3cfg mgr (putObject bucket k (RequestBodyBS content))))
      return (cfg, s3cfg, mgr)
    teardown (cfg, s3cfg, mgr) =
      void (runResourceT (pureAws cfg s3cfg mgr (DeleteObject k bucket)))


-------------------------------------------------------------------------------
-- GetObject Tests
-------------------------------------------------------------------------------


test_get :: TestTree
test_get = askOption $ \(BucketOption bucket) -> testGroup "GetObject"
  [ test_get_caching bucket
  ]


test_get_caching :: Bucket -> TestTree
test_get_caching bucket = withResource mkSetup teardown $ \setup -> testGroup "Caches"
  [ testCase "If-Matches match succeeds" $ do
      (cfg, s3cfg, mgr) <- setup
      void (runResourceT (pureAws cfg s3cfg mgr (getObject bucket k) { goIfMatch = Just payloadMD5 }))
  , testCase "If-Matches mismatch fails with 412" $ do
      (cfg, s3cfg, mgr) <- setup
      assertStatus 412 (runResourceT (pureAws cfg s3cfg mgr (getObject bucket k) { goIfMatch = Just (T.reverse payloadMD5) }))
  , testCase "If-None-Match mismatch succeeds" $ do
      (cfg, s3cfg, mgr) <- setup
      void (runResourceT (pureAws cfg s3cfg mgr (getObject bucket k) { goIfNoneMatch = Just (T.reverse payloadMD5) }))
  , testCase "If-None-Match match fails with 304" $ do
      (cfg, s3cfg, mgr) <- setup
      assertStatus 304 (runResourceT (pureAws cfg s3cfg mgr (getObject bucket k) { goIfNoneMatch = Just payloadMD5 }))
  ]
  where
    k = "s3-test-object"
    content = "example"
    payloadMD5 = "1a79a4d60de6718e8e5b326e338ae533"
    mkSetup = do
      cfg <- baseConfiguration
      let s3cfg = defServiceConfig
      mgr <- newManager tlsManagerSettings
      void (runResourceT (pureAws cfg s3cfg mgr (putObject bucket k (RequestBodyBS content))))
      return (cfg, s3cfg, mgr)
    teardown (cfg, s3cfg, mgr) =
      void (runResourceT (pureAws cfg s3cfg mgr (DeleteObject k bucket)))


assertStatus :: Int -> IO a -> Assertion
assertStatus expectedStatus f = do
  res <- catchJust selector
                   (Right <$> f)
                   (return . Left)
  case res of
    Right _ -> assertFailure ("Expected error with status " <> show expectedStatus <> ", but got success.")
    Left _ -> return ()
  where
    selector (StatusCodeException s _ _)
      | statusCode s == expectedStatus = Just ()
      | otherwise = Nothing
    selector  _ = Nothing
