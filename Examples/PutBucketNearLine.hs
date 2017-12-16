-- | Example of creating a Nearline bucket on Google Cloud Storage.

{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (withManager, RequestBody(..))
import Control.Monad.IO.Class
import Control.Concurrent
import System.IO
import Control.Applicative
import qualified Data.Text as T
import System.Environment

sc :: S3.StorageClass
sc = S3.OtherStorageClass (T.pack "NEARLINE")

main :: IO ()
main = do
  [bucket] <- fmap (map T.pack) getArgs

  {- Set up AWS credentials and S3 configuration using the Google Cloud
   - Storage endpoint. -}
  Just creds <- Aws.loadCredentialsFromEnv
  let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
  let s3cfg = S3.s3 Aws.HTTP "storage.googleapis.com" False

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    {- Create a request object with S3.PutBucket and run the request with pureAws. -}
    rsp <-
      Aws.pureAws cfg s3cfg mgr $
        S3.PutBucket bucket Nothing "US" (Just sc)
    liftIO $ print rsp
