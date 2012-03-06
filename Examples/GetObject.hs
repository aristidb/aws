{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import Data.Conduit
import Data.Conduit.Binary
import Data.IORef
import Data.Monoid

-- A small function to save the object's data into a file.
saveObject :: Aws.HTTPResponseConsumer ()
saveObject status headers source = source $$ sinkFile "cloud-remote.pdf"

main :: IO ()
main = do
  -- Set up AWS credentials and the default configuration.
  cfg <- Aws.baseConfiguration

  -- Create an IORef to store the response Metadata (so it is also available in case of an error).
  metadataRef <- newIORef mempty

  -- Create a request object with S3.getObject and run the request with simpleAwsRef.
  Aws.simpleAwsRef cfg metadataRef $ S3.getObject "haskell-aws" "cloud-remote.pdf" saveObject

  -- Print the response metadata.
  print =<< readIORef metadataRef
