{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, RecordWildCards, TypeFamilies, RankNTypes #-}
module Aws.S3.Response
where

import           Aws.Response
import           Aws.S3.Error
import           Aws.S3.Metadata
import           Aws.Util
import           Aws.Xml
import           Control.Monad.IO.Class
import           Data.Attempt                 (Attempt(..))
import           Data.Conduit                 (($$))
import           Data.IORef
import           Data.Maybe
import           Text.XML.Cursor              (($/))
import qualified Data.ByteString              as B
import qualified Data.Conduit                 as C
import qualified Data.Text.Encoding           as T
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Cursor              as Cu
import qualified Text.XML                     as XML

s3ResponseConsumer :: HTTPResponseConsumer a
                   -> IORef S3Metadata
                   -> HTTPResponseConsumer a
s3ResponseConsumer inner metadata status headers source = do
      let headerString = fmap T.decodeUtf8 . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"

      let m = S3Metadata { s3MAmzId2 = amzId2, s3MRequestId = requestId }
      liftIO $ tellMetadataRef metadata m

      if status >= HTTP.status400
        then s3ErrorResponseConsumer status headers source
        else inner status headers source

s3XmlResponseConsumer :: (Cu.Cursor -> Response S3Metadata a)
                      -> IORef S3Metadata
                      -> HTTPResponseConsumer a
s3XmlResponseConsumer parse metadataRef =
    s3ResponseConsumer (xmlCursorConsumer parse metadataRef) metadataRef

s3BinaryResponseConsumer :: HTTPResponseConsumer a
                         -> IORef S3Metadata
                         -> HTTPResponseConsumer a
s3BinaryResponseConsumer inner metadataRef = s3ResponseConsumer inner metadataRef

s3ErrorResponseConsumer :: HTTPResponseConsumer a
s3ErrorResponseConsumer status _headers source
    = do doc <- source $$ XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         liftIO $ case parseError cursor of
           Success err      -> C.monadThrow err
           Failure otherErr -> C.monadThrow otherErr
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
