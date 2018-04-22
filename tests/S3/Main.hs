{-# LANGUAGE CPP                        #-}
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
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import           Conduit
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
                                               RequestBody (..), newManager,
                                               responseBody)
#if MIN_VERSION_http_client(0, 5, 0)
import           Network.HTTP.Client          (HttpExceptionContent (..),
                                               responseStatus)
#endif
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
    , test_versioning
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


-------------------------------------------------------------------------------
-- GetBucketObjectVersions Tests
-------------------------------------------------------------------------------


test_versioning :: TestTree
test_versioning = askOption $ \(BucketOption bucket) ->
  withResource (mkSetup bucket) (teardown bucket) $ \setup -> testGroup "Versioning"
    [ testCase "GetBucketObjectVersions succeeds" $ do
        (cfg, s3cfg, mgr) <- setup
        resp <- runResourceT $ pureAws cfg s3cfg mgr $ (getBucketObjectVersions bucket)
          { gbovPrefix = Just k
          }
        let [o1, o2, o3, o4] = take 4 $ gbovrContents resp
        checkObject True o1
        checkDeleteMarker False o2
        checkObject False o3
        checkObject False o4
    , testCase "DeleteObjectVersion succeeds" $ do
        -- Note: this test requires bucket with versioning enabled
        (cfg, s3cfg, mgr) <- setup
        resp <- runResourceT $ pureAws cfg s3cfg mgr $ (getBucketObjectVersions bucket)
          { gbovPrefix = Just k
          }
        let [v1, v2, v3, v4] = map oviVersionId $ take 4 $ gbovrContents resp
        void (runResourceT (pureAws cfg s3cfg mgr (deleteObjectVersion bucket k v2)))
        void (runResourceT (pureAws cfg s3cfg mgr (deleteObjectVersion bucket k v3)))

        resp' <- runResourceT $ pureAws cfg s3cfg mgr $ (getBucketObjectVersions bucket)
          { gbovPrefix = Just k
          }
        let [v1', v4'] = map oviVersionId $ take 2 $ gbovrContents resp'
        assertEqual "invalid v1 version" v1 v1'
        assertEqual "invalid v4 version" v4 v4'
    , testCase "Multipart upload succeeds" $ do
        -- Note: this test requires bucket with versioning enabled
        (cfg, s3cfg, mgr) <- setup
        resp <- runResourceT $ do
            uploadId <- liftIO $ getUploadId cfg s3cfg mgr bucket k
            etags <- sourceLazy testStr
                $= chunkedConduit 65536
                $= putConduit cfg s3cfg mgr bucket k uploadId
                $$ sinkList
            liftIO $ sendEtag cfg s3cfg mgr bucket k uploadId etags
        let Just vid = cmurVersionId resp
        bs <- runResourceT $ do
            gor <- pureAws cfg s3cfg mgr (getObject bucket k) { goVersionId = Just vid }
            sealConduitT (responseBody (gorResponse gor)) $$+- sinkLazy

        assertEqual "data do not match" testStr bs
    ]
  where
    testStr = "foobar" :: BL.ByteString
    k = "s3-test-object"
    content = "example"
    payloadMD5 = "1a79a4d60de6718e8e5b326e338ae533"
    checkObject marker obj@ObjectVersion{} = do
        assertEqual "invalid object key" k (oviKey obj)
        assertEqual "invalid isLatest flag" marker (oviIsLatest obj)
        assertEqual "invalid object size" (fromIntegral $ BS.length content) (oviSize obj)
    checkObject _ obj = assertFailure $ "Invalid object type " <> show obj
    checkDeleteMarker marker obj@DeleteMarker{} = do
        assertEqual "invalid object key" k (oviKey obj)
        assertEqual "invalid isLatest flag" marker (oviIsLatest obj)
    checkDeleteMarker _ obj = assertFailure $ "Invalid object type " <> show obj
    mkSetup bucket = do
      cfg <- baseConfiguration
      let s3cfg = defServiceConfig
      mgr <- newManager tlsManagerSettings
      void (runResourceT (pureAws cfg s3cfg mgr (putObject bucket k (RequestBodyBS content))))
      void (runResourceT (pureAws cfg s3cfg mgr (putObject bucket k (RequestBodyBS content))))
      void (runResourceT (pureAws cfg s3cfg mgr (DeleteObject k bucket)))
      void (runResourceT (pureAws cfg s3cfg mgr (putObject bucket k (RequestBodyBS content))))
      return (cfg, s3cfg, mgr)
    teardown bucket (cfg, s3cfg, mgr) =
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
#if MIN_VERSION_http_client(0, 5, 0)
    selector (HttpExceptionRequest _ (StatusCodeException res _))
      | statusCode (responseStatus res) == expectedStatus = Just ()
    selector _ = Nothing
#else
    selector (StatusCodeException s _ _)
      | statusCode s == expectedStatus = Just ()
      | otherwise = Nothing
    selector  _ = Nothing
#endif
