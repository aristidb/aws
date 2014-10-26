{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit (($$))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Client (withManager, responseBody, defaultManagerSettings)

main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager defaultManagerSettings $ \mgr -> do
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.GetObjectResponse { S3.gorResponse = rsp } <-
      Aws.pureAws cfg s3cfg mgr $
        S3.getObject "haskell-aws" "cloud-remote.pdf"

    {- Save the response to a file. -}
    runResourceT $ Aws.bodyReaderSource (responseBody rsp) $$ sinkFile "cloud-remote.pdf"
