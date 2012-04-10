{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Aws.Sqs.Response where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Metadata
import           Aws.Xml
import           Control.Monad.IO.Class
import           Data.Attempt                 (Attempt(..))
import           Data.Conduit                 (($$))
import           Data.IORef
import           Data.Maybe
import           Text.XML.Cursor              (($/))
import qualified Data.Conduit                 as C
import qualified Data.Text.Encoding           as T
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Cursor              as Cu
import qualified Text.XML                     as XML

sqsResponseConsumer :: HTTPResponseConsumer a
                    -> IORef SqsMetadata
                    -> HTTPResponseConsumer a
sqsResponseConsumer inner metadata status headers source = do
      let headerString = fmap T.decodeUtf8 . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"

      let m = SqsMetadata { sqsMAmzId2 = amzId2, sqsMRequestId = requestId }
      liftIO $ tellMetadataRef metadata m

      if status >= HTTP.status400
        then sqsErrorResponseConsumer status headers source
        else inner status headers source

sqsXmlResponseConsumer :: (Cu.Cursor -> Response SqsMetadata a)
                       -> IORef SqsMetadata
                       -> HTTPResponseConsumer a
sqsXmlResponseConsumer parse metadataRef = sqsResponseConsumer (xmlCursorConsumer parse metadataRef) metadataRef

sqsErrorResponseConsumer :: HTTPResponseConsumer a
sqsErrorResponseConsumer status _headers source
    = do doc <- source $$ XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         liftIO $ case parseError cursor of
           Success err -> C.monadThrow err
           Failure otherErr -> C.monadThrow otherErr
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
