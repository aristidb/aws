module Aws.S3.Commands.MultipartUpload (
  multipartUpload
  ) where

import           Aws.Aws
import           Aws.Core
import qualified Aws.S3.Core as S3
import qualified Aws.S3.Commands.InitiateMultipartUpload as S3
import qualified Aws.S3.Commands.CompleteMultipartUpload as S3
import qualified Aws.S3.Commands.UploadPart as S3
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import           Text.Printf(printf)

import           Data.Conduit
import qualified Data.Conduit.Binary   as CB
import qualified Data.Conduit.List     as CL
import           Network.HTTP.Conduit
import           Control.Monad.Trans.Resource

import qualified Crypto.Hash.MD5       as MD5


getUploadId ::
  Configuration
  -> S3.S3Configuration NormalQuery
  -> Manager
  -> T.Text
  -> T.Text
  -> ResourceT IO T.Text
getUploadId cfg s3cfg mgr bucket object = do
  S3.InitiateMultipartUploadResponse {
      S3.imurBucket = _bucket
    , S3.imurKey = _object'
    , S3.imurUploadId = uploadId
    } <- pureAws cfg s3cfg mgr $ S3.postInitiateMultipartUpload bucket object
  return uploadId


sendEtag  ::
  Configuration
  -> S3.S3Configuration NormalQuery
  -> Manager
  -> T.Text
  -> T.Text
  -> T.Text
  -> [String]
  -> ResourceT IO ()
sendEtag cfg s3cfg mgr bucket object uploadId etags = do
  _ <- pureAws cfg s3cfg mgr $
       S3.postCompleteMultipartUpload bucket object uploadId (zip [1..] (map T.pack etags))
  return ()


bstr2str :: B8.ByteString -> String
bstr2str bstr =
  foldr1 (++) $ map toHex $ B8.unpack bstr
  where
    toHex :: Char -> String
    toHex chr = printf "%02x" chr

putConduit ::
  MonadResource m =>
  Configuration
  -> S3.S3Configuration NormalQuery
  -> Manager
  -> T.Text
  -> T.Text
  -> T.Text
  -> Conduit B8.ByteString m String
putConduit cfg s3cfg mgr bucket object uploadId = loop 1
  where
    loop n = do
      v' <- await
      case v' of
        Just v -> do
          let str= (BL.fromStrict v)
          _ <- liftResourceT $ pureAws cfg s3cfg mgr $
            S3.uploadPart bucket object n uploadId
            (requestBodySource
             (BL.length str)
             (CB.sourceLbs str)
            )
          let etag= bstr2str $ MD5.hash v
          yield etag
          loop (n+1)
        Nothing -> return ()

chunkedConduit :: (MonadResource m) => Integer -> Conduit B8.ByteString m B8.ByteString
chunkedConduit size = do
  loop 0 ""
  where
    loop cnt str = do
      line' <- await
      case line' of 
        Nothing -> do
          yield str
          return ()
        Just line -> do
          let len = (B8.length line)+cnt
          let newStr = B8.concat [str, line]
          if len >= (fromIntegral size)
            then do
            yield newStr
            loop 0 ""
            else
            loop len newStr

multipartUpload :: Configuration
                   -> S3.S3Configuration NormalQuery
                   -> Manager
                   -> T.Text
                   -> T.Text
                   -> Conduit () (ResourceT IO) B8.ByteString
                   -> Integer
                   -> ResourceT IO ()
multipartUpload cfg s3cfg mgr bucket object src chunkSize = do
  uploadId <- getUploadId cfg s3cfg mgr bucket object
  etags <- src
           $= chunkedConduit chunkSize
           $= putConduit cfg s3cfg mgr bucket object uploadId
           $$ CL.consume
  sendEtag cfg s3cfg mgr bucket object uploadId etags
