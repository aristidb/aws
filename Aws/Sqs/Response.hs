{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Aws.Sqs.Response where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Metadata
import           Aws.Xml
import           Control.Monad.IO.Class
import           Data.Attempt                 (Attempt(..))
import           Data.Enumerator              ((=$))
import           Data.IORef
import           Data.Maybe
import           Text.XML.Enumerator.Cursor   (($/))
import qualified Data.ByteString              as B
import qualified Data.Enumerator              as En
import qualified Data.Text.Encoding           as T
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

sqsResponseIteratee ::
    (HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a)
    -> IORef SqsMetadata
    -> HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
sqsResponseIteratee inner metadata status headers = do
      let headerString = fmap T.decodeUtf8 . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"
      
      let m = SqsMetadata { sqsMAmzId2 = amzId2, sqsMRequestId = requestId }
      liftIO $ tellMetadataRef metadata m
      
      if status >= HTTP.status400
        then sqsErrorResponseIteratee status headers
        else inner status headers

sqsXmlResponseIteratee :: 
    (Cu.Cursor -> Response SqsMetadata a)
    -> IORef SqsMetadata
    -> HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
sqsXmlResponseIteratee parse metadataRef = sqsResponseIteratee (xmlCursorIteratee parse metadataRef) metadataRef

sqsErrorResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
sqsErrorResponseIteratee status _headers 
    = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
         let cursor = Cu.fromDocument doc
         case parseError cursor of
           Success err -> En.throwError err
           Failure otherErr -> En.throwError otherErr
    where
      parseError :: Cu.Cursor -> Attempt SqsError
      parseError root = do cursor <- force "Missing Error" $ root $/ Cu.laxElement "Error" 
                           code <- force "Missing error Code" $ cursor $/ elContent "Code"
                           message <- force "Missing error Message" $ cursor $/ elContent "Message"
                           errorType <- force "Missing error Type" $ cursor $/ elContent "Type"
                           let detail = listToMaybe $ cursor $/ elContent "Detail"
                           
                           return SqsError {
                                        sqsStatusCode = status
                                      , sqsErrorCode = code
                                      , sqsErrorMessage = message
                                      , sqsErrorType = errorType
                                      , sqsErrorDetail = detail
                                      , sqsErrorMetadata = Nothing
                                      }
