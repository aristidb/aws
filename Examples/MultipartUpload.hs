{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sourceFile)
import           Network.HTTP.Conduit (withManager, responseBody)


main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    S3.multipartUpload cfg s3cfg mgr "haskell-aws" "cloud-remote.pdf" (sourceFile "cloud-local.pdf") (10*1024*1024)
