{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Aws.S3.Response
where

import           Aws.Metadata
import           Aws.Response
import           Aws.S3.Error
import           Aws.S3.Metadata
import           Aws.Util
import           Control.Applicative
import           Control.Monad.Compose.Class
import           Data.Char
import           Data.Enumerator              ((=$))
import           Data.Maybe
import           Data.Word
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.Enumerator              as En
import qualified Network.HTTP.Enumerator      as HTTPE
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Enumerator.Cursor   as XML
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

data S3Response a
    = S3Response {
        fromS3Response :: a
      , s3AmzId2 :: String
      , s3RequestId :: String
      }
    deriving (Show)

instance (S3ResponseIteratee a) => ResponseIteratee (S3Response a) where
    responseIteratee status headers = do
      let headerString = fromMaybe "" . fmap B8.unpack . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"
      
      specific <- tryError $ if status >= HTTP.status400
                             then s3ErrorResponseIteratee status headers
                             else s3ResponseIteratee status headers
      
      case specific of
        Left (err :: S3Error) -> En.throwError (setMetadata m err)
            where m = S3Metadata { s3MAmzId2 = amzId2, s3MRequestId = requestId }
        Right resp -> return S3Response {
                                        fromS3Response = resp
                                      , s3AmzId2 = amzId2
                                      , s3RequestId = requestId
                                      }

s3ErrorResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
s3ErrorResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = XML.fromDocument doc
                                            En.throwError (parseError cursor)
    where parseError _ = S3Error {}

{-
s3ErrorResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
s3ErrorResponseIteratee status headers = xmlResponseIteratee (e <<< parseXmlResponse) status headers
    where                 
      e :: Xml S3Error XL.Element a
      e = do
        err <- e' <<< findElementNameUI "Error"
        raise err
      
      e' = do
        code <- strContent <<< findElementNameUI "Code"
        message <- strContent <<< findElementNameUI "Message"
        resource <- tryMaybe $ strContent <<< findElementNameUI "Resource"
        hostId <- tryMaybe $ strContent <<< findElementNameUI "HostId"
        accessKeyId <- tryMaybe $ strContent <<< findElementNameUI "AWSAccessKeyId"
        stringToSignUnprocessed <- tryMaybe $ strContent <<< findElementNameUI "StringToSignBytes"
        let stringToSign = B.pack <$> (sequence . map readHex2 . words =<< stringToSignUnprocessed)
        
        return S3Error { 
                     s3StatusCode = status
                   , s3ErrorCode = code
                   , s3ErrorMessage = message
                   , s3ErrorResource = resource
                   , s3ErrorHostId = hostId
                   , s3ErrorAccessKeyId = accessKeyId
                   , s3ErrorStringToSign = stringToSign
                   , s3ErrorMetadata = Nothing
                   }

      readHex2 :: [Char] -> Maybe Word8
      readHex2 [c1,c2] = do
        n1 <- readHex1 c1
        n2 <- readHex1 c2
        return . fromIntegral $ n1 * 16 + n2
      readHex2 _ = Nothing
      
      readHex1 c | c >= '0' && c <= '9' = Just $ ord c - ord '0'
                 | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
                 | c >= 'a' && c <= 'f' = Just $ ord c - ord 'a' + 10
      readHex1 _                        = Nothing
-}

class S3ResponseIteratee a where
    s3ResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a

instance S3ResponseIteratee HTTPE.Response where
    s3ResponseIteratee = HTTPE.lbsIter
