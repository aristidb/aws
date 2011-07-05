{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, RecordWildCards, TypeFamilies #-}
module Aws.S3.Response
where

import           Aws.Response
import           Aws.S3.Error
import           Aws.S3.Metadata
import           Aws.Util
import           Aws.Xml
import           Control.Monad.IO.Class
import           Data.Attempt                 (Attempt(..))
import           Data.Char
import           Data.Enumerator              ((=$))
import           Data.IORef
import           Data.Maybe
import           Data.Word
import           Text.XML.Enumerator.Cursor   (($/))
import qualified Data.ByteString              as B
import qualified Data.Enumerator              as En
import qualified Data.Text.Encoding           as T
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

s3ResponseIteratee ::
    (HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a)
    -> IORef S3Metadata
    -> HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
s3ResponseIteratee inner metadata status headers = do
      let headerString = fmap T.decodeUtf8 . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"
      
      let m = S3Metadata { s3MAmzId2 = amzId2, s3MRequestId = requestId }
      liftIO $ tellMetadataRef metadata m
      
      if status >= HTTP.status400
        then s3ErrorResponseIteratee status headers
        else inner status headers

s3XmlResponseIteratee :: 
    (Cu.Cursor -> Response S3Metadata a)
    -> IORef S3Metadata
    -> HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
s3XmlResponseIteratee parse metadataRef = s3ResponseIteratee (xmlCursorIteratee parse metadataRef) metadataRef

s3ErrorResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
s3ErrorResponseIteratee status _headers 
    = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
         let cursor = Cu.fromDocument doc
         case parseError cursor of
           Success err -> En.throwError err
           Failure otherErr -> En.throwError otherErr
    where
      parseError :: Cu.Cursor -> Attempt S3Error
      parseError root = do code <- force "Missing error Code" $ root $/ elContent "Code"
                           message <- force "Missing error Message" $ root $/ elContent "Message"
                           let resource = listToMaybe $ root $/ elContent "Resource"
                               hostId = listToMaybe $ root $/ elContent "HostId"
                               accessKeyId = listToMaybe $ root $/ elContent "AWSAccessKeyId"
                               stringToSign = do unprocessed <- listToMaybe $ root $/ elCont "StringToSignBytes"
                                                 bytes <- mapM readHex2 $ words unprocessed
                                                 return $ B.pack bytes
                           return S3Error {
                                        s3StatusCode = status
                                      , s3ErrorCode = code
                                      , s3ErrorMessage = message
                                      , s3ErrorResource = resource
                                      , s3ErrorHostId = hostId
                                      , s3ErrorAccessKeyId = accessKeyId
                                      , s3ErrorStringToSign = stringToSign
                                      }
