{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Data.IORef (newIORef, readIORef)
import           Data.Monoid (mempty)
import           Network.HTTP.Conduit (withManager, responseBody)

main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration

  {- Create an IORef to store the response Metadata (so it is also available in case of an error). -}
  metadataRef <- newIORef mempty

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    {- Create a request object with S3.getObject and run the request with awsRef. -}
    S3.GetObjectResponse { S3.gorResponse = rsp } <-
      Aws.awsRef cfg Aws.defServiceConfig mgr metadataRef $
        S3.getObject "haskell-aws" "cloud-remote.pdf"

    {- Save the response to a file. -}
    responseBody rsp $$+- sinkFile "cloud-remote.pdf"

  {- Print the response metadata. -}
  print =<< readIORef metadataRef
