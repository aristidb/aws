{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (responseBody)

main :: IO ()
main = Aws.withDefaultEnvironment $ \env -> runResourceT $ do
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.GetObjectResponse { S3.gorResponse = rsp } <-
      Aws.pureAws env $ S3.getObject "haskell-aws" "cloud-remote.pdf"

    {- Save the response to a file. -}
    responseBody rsp $$+- sinkFile "cloud-remote.pdf"
