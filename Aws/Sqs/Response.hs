{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
module Aws.Sqs.Response
where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Metadata
import           Aws.Util
import           Aws.Xml
import           Control.Applicative
import           Control.Monad.Compose.Class
import           Data.Char
import           Data.Enumerator              ((=$))
import           Data.Maybe
import           Data.Word
import           Text.XML.Enumerator.Cursor   (($/), (&|), (&/), ($//))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Network.HTTP.Enumerator      as HTTPE
import qualified Network.HTTP.Types           as HTTP
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

data SqsResponse a
    = SqsResponse { 
        fromSqsResponse :: a
      , sqsAmzId2 :: String
      , sqsRequestId :: String }
    deriving (Show)

--instance Functor SqsResponse where
--    fmap f (SqsResponse a m) = SqsResponse (f a) m

instance (SqsResponseIteratee a) => ResponseIteratee (SqsResponse a) where
      responseIteratee status headers = do
      let headerString = fromMaybe "" . fmap B8.unpack . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"
      
      specific <- tryError $ if status >= HTTP.status400
                             then sqsErrorResponseIteratee status headers
                             else sqsResponseIteratee status headers
      
      case specific of
        Left (err :: SqsError) -> En.throwError (setMetadata m err)
            where m = SqsMetadata { sqsMAmzId2 = amzId2, sqsMRequestId = requestId }
        Right resp -> return SqsResponse {
                                        fromSqsResponse = resp
                                      , sqsAmzId2 = amzId2
                                      , sqsRequestId = requestId
                                      }


sqsErrorResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
sqsErrorResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                             --print $ XML.renderLBS doc
                                             let cursor = Cu.fromDocument doc
                                             case parseError cursor of
                                               Left invalidXml -> En.throwError invalidXml
                                               Right err -> En.throwError err
    where
      parseError :: Cu.Cursor -> Either SqsError SqsError
      parseError root = do code <- sqsForce "Missing error Code" $ root $// elCont "Code"
                           message <- sqsForce "Missing error Message" $ root $// elCont "Message"
                           let resource = listToMaybe $ root $/ elCont "Resource"
                               hostId = listToMaybe $ root $/ elCont "HostId"
                               accessKeyId = listToMaybe $ root $/ elCont "AWSAccessKeyId"
                               stringToSign = do unprocessed <- listToMaybe $ root $/ elCont "StringToSignBytes"
                                                 bytes <- mapM readHex2 $ words unprocessed
                                                 return $ B.pack bytes
                           return SqsError {
                                        sqsStatusCode = status
                                      , sqsErrorCode = code
                                      , sqsErrorMessage = message
                                      , sqsErrorMetadata = Nothing
                                      }
          where readHex2 :: [Char] -> Maybe Word8
                readHex2 [c1,c2] = do n1 <- readHex1 c1
                                      n2 <- readHex1 c2
                                      return . fromIntegral $ n1 * 16 + n2
                readHex2 _ = Nothing
      
                readHex1 c | c >= '0' && c <= '9' = Just $ ord c - ord '0'
                           | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
                           | c >= 'a' && c <= 'f' = Just $ ord c - ord 'a' + 10
                readHex1 _                        = Nothing



class SqsResponseIteratee a where
    sqsResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a

instance SqsResponseIteratee HTTPE.Response where
    sqsResponseIteratee = HTTPE.lbsIter
