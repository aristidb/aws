{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import qualified Data.ByteString.Char8 as B
import           Data.Conduit (connect)
import           Data.Conduit.Binary (sourceFile)
import qualified Data.Text as T
import           Network.HTTP.Conduit (newManager, tlsManagerSettings, responseBody)
import           Control.Monad.Trans.Resource (runResourceT)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [endpoint, bucket, obj, file]            -> doUpload endpoint bucket obj file 10
    [endpoint, bucket, obj, file, chunkSize] -> doUpload endpoint bucket obj file (read chunkSize)
    _ -> mapM_ putStrLn
      [ "Usage: MultipartUpload endpoint bucket dstobject srcfile [chunksize(MB)]"
      , "Example: MultipartUpload s3.us-east-2.amazonaws.com your-bucket tmp/test.bin test.bin"
      ]
  where
    doUpload endpoint bucket obj file chunkSize = do
      cfg <- Aws.dbgConfiguration
      let s3cfg = S3.s3v4 Aws.HTTPS (B.pack endpoint) False S3.SignWithEffort
      mgr <- newManager tlsManagerSettings
      runResourceT $
        sourceFile file `connect` S3.multipartUploadSink cfg s3cfg mgr (T.pack bucket) (T.pack obj) (chunkSize*1024*1024)
