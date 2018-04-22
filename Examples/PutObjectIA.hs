{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings, RequestBody(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Concurrent
import System.Posix.Files
import System.IO
import Control.Applicative
import qualified Data.Text as T

main :: IO ()
main = do
  {- Set up AWS credentials and S3 configuration using the IA endpoint. -}
  Just creds <- Aws.loadCredentialsFromEnv
  let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
  let s3cfg = S3.s3 Aws.HTTP "s3.us.archive.org" False

  {- Set up a ResourceT region with an available HTTP manager. -}
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    let file ="test"
    -- streams large file content, without buffering more than 10k in memory
    let streamer sink = withFile file ReadMode $ \h -> sink $ S.hGet h 10240
    b <- liftIO $ L.readFile file
    size <- liftIO $ (fromIntegral . fileSize <$> getFileStatus file :: IO Integer)
    let body = RequestBodyStream (fromInteger size) streamer
    rsp <- Aws.pureAws cfg s3cfg mgr $
        (S3.putObject "joeyh-test-item" (T.pack file) body)
		{ S3.poMetadata =
			[ ("mediatype", "texts")
			, ("meta-description", "test Internet Archive item made via haskell aws library")
			]
		-- Automatically creates bucket on IA if it does not exist,
		-- and uses the above metadata as the bucket's metadata.
		, S3.poAutoMakeBucket = True
		}
    liftIO $ print rsp
