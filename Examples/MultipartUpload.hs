{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit.Binary (sourceFile)
import qualified Data.Text as T
import           Network.HTTP.Client (withManager, responseBody, defaultManagerSettings)
import           System.Environment (getArgs)

main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  args <- getArgs

  let doUpload bucket obj file chunkSize =
        withManager defaultManagerSettings $ \mgr -> runResourceT $ do
          S3.multipartUpload cfg s3cfg mgr (T.pack bucket) (T.pack obj) (sourceFile file) (chunkSize*1024*1024)

  case args of
    [bucket,obj,file] -> 
      doUpload bucket obj file 10
    [bucket,obj,file,chunkSize] -> 
      doUpload bucket obj file (read chunkSize)
    _ -> do
      putStrLn "Usage: MultipartUpload bucket objectname filename (chunksize(MB)::optinal)"
