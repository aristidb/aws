{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import           Control.Monad.Trans.Resource
import           Data.Conduit ((.|), runConduit)
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings, responseBody)

main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  Just creds <- Aws.loadCredentialsDefault
  let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
  let s3cfg = S3.s3v4 Aws.HTTP "s3.amazonaws.com" False S3.SignWithEffort

  {- Set up a ResourceT region with an available HTTP manager. -}
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.GetObjectResponse { S3.gorResponse = rsp } <-
      Aws.pureAws cfg s3cfg mgr $
        S3.getObject "haskell-aws" "cloud-remote.pdf"

    {- Save the response to a file. -}
    runConduit $ responseBody rsp .| sinkFile "cloud-remote.pdf"
